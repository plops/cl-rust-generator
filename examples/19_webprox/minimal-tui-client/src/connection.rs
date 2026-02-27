use proto_definitions::browser::browsing_service_client::BrowsingServiceClient;
use proto_definitions::browser::*;
use std::time::Duration;
use tokio::sync::mpsc;
use tokio_stream::wrappers::ReceiverStream;
use tonic::transport::Channel;

const INITIAL_BACKOFF: Duration = Duration::from_millis(500);
const MAX_BACKOFF: Duration = Duration::from_secs(10);
const MAX_RETRIES: u32 = 5;

/// Connection state
#[derive(Debug, Clone, PartialEq)]
pub enum ConnectionState {
    Connected,
    Reconnecting(u32), // attempt number
    Offline,
}

/// Manages the gRPC connection with reconnection logic
pub struct ServerConnection {
    addr: String,
    pub state: ConnectionState,
    last_url: Option<String>,
}

impl ServerConnection {
    pub fn new(addr: String) -> Self {
        Self {
            addr,
            state: ConnectionState::Offline,
            last_url: None,
        }
    }

    /// Connect and establish a bidirectional stream.
    /// Returns channels for sending interactions and receiving page updates.
    pub async fn connect(
        &mut self,
    ) -> Result<
        (
            mpsc::Sender<Interaction>,
            tonic::Streaming<PageUpdate>,
        ),
        Box<dyn std::error::Error>,
    > {
        let channel = Channel::from_shared(self.addr.clone())?
            .connect()
            .await?;

        let mut client = BrowsingServiceClient::new(channel);

        let (interaction_tx, interaction_rx) = mpsc::channel::<Interaction>(32);
        let stream = ReceiverStream::new(interaction_rx);

        let response = client.session(stream).await?;
        let update_stream = response.into_inner();

        self.state = ConnectionState::Connected;
        Ok((interaction_tx, update_stream))
    }

    /// Attempt reconnection with exponential backoff
    pub async fn reconnect(
        &mut self,
    ) -> Result<
        (
            mpsc::Sender<Interaction>,
            tonic::Streaming<PageUpdate>,
        ),
        Box<dyn std::error::Error>,
    > {
        let mut backoff = INITIAL_BACKOFF;

        for attempt in 1..=MAX_RETRIES {
            self.state = ConnectionState::Reconnecting(attempt);
            eprintln!("Reconnection attempt {}/{}", attempt, MAX_RETRIES);

            tokio::time::sleep(backoff).await;

            match self.connect().await {
                Ok((tx, stream)) => {
                    // Resync: navigate to last known URL
                    if let Some(ref url) = self.last_url {
                        let _ = tx
                            .send(Interaction {
                                r#type: Some(interaction::Type::Navigate(NavigateRequest {
                                    url: url.clone(),
                                    session_id: String::new(),
                                })),
                            })
                            .await;
                    }
                    return Ok((tx, stream));
                }
                Err(e) => {
                    eprintln!("Reconnect attempt {} failed: {}", attempt, e);
                    backoff = (backoff * 2).min(MAX_BACKOFF);
                }
            }
        }

        self.state = ConnectionState::Offline;
        Err("Max reconnection attempts exceeded".into())
    }

    pub fn set_last_url(&mut self, url: String) {
        self.last_url = Some(url);
    }

    pub fn status_text(&self) -> &str {
        match &self.state {
            ConnectionState::Connected => "Connected",
            ConnectionState::Reconnecting(_) => "Reconnecting...",
            ConnectionState::Offline => "Offline",
        }
    }
}
