use crate::browser::BrowserPool;
use crate::extractor::ExtractionPipeline;
use crate::input::InputDispatcher;
use crate::session::SessionManager;

use futures_util::StreamExt;
use proto_definitions::browser::browsing_service_server::BrowsingService;
use proto_definitions::browser::*;
use std::pin::Pin;
use std::sync::Arc;
use tokio::sync::mpsc;
use tokio_stream::wrappers::ReceiverStream;
use tokio_stream::Stream;
use tonic::{Request, Response, Status, Streaming};

fn generate_session_id() -> String {
    format!("{:016x}{:016x}", fastrand::u64(..), fastrand::u64(..))
}

pub struct BrowserBackend {
    browser_pool: Arc<BrowserPool>,
    session_manager: SessionManager,
}

impl BrowserBackend {
    pub async fn new(
        session_manager: SessionManager,
    ) -> Result<Self, Box<dyn std::error::Error + Send + Sync>> {
        let browser_pool = Arc::new(BrowserPool::new().await?);
        Ok(Self {
            browser_pool,
            session_manager,
        })
    }
}

#[tonic::async_trait]
impl BrowsingService for BrowserBackend {
    type SessionStream =
        Pin<Box<dyn Stream<Item = Result<PageUpdate, Status>> + Send + 'static>>;

    async fn session(
        &self,
        request: Request<Streaming<Interaction>>,
    ) -> Result<Response<Self::SessionStream>, Status> {
        let mut in_stream = request.into_inner();
        let (tx, rx) = mpsc::channel(128);

        let browser_pool = Arc::clone(&self.browser_pool);
        let session_manager = self.session_manager.clone();

        tokio::spawn(async move {
            let mut session_id: Option<String> = None;

            while let Some(result) = in_stream.next().await {
                let interaction = match result {
                    Ok(i) => i,
                    Err(e) => {
                        tracing::error!("Stream error: {}", e);
                        break;
                    }
                };

                let r#type = match interaction.r#type {
                    Some(t) => t,
                    None => continue,
                };

                match r#type {
                    interaction::Type::Navigate(nav) => {
                        let sid = if nav.session_id.is_empty() {
                            let new_id = generate_session_id();
                            // Create a new page for this session
                            match browser_pool.new_page().await {
                                Ok(page) => {
                                    session_manager
                                        .create_session(new_id.clone(), page)
                                        .await;
                                    session_id = Some(new_id.clone());
                                    new_id
                                }
                                Err(e) => {
                                    let _ = tx
                                        .send(Ok(PageUpdate {
                                            content: Some(page_update::Content::Status(
                                                StatusMessage {
                                                    message: format!(
                                                        "Failed to create session: {}",
                                                        e
                                                    ),
                                                    level: StatusLevel::Error.into(),
                                                },
                                            )),
                                        }))
                                        .await;
                                    continue;
                                }
                            }
                        } else {
                            nav.session_id.clone()
                        };

                        // Send loading status
                        let _ = tx
                            .send(Ok(PageUpdate {
                                content: Some(page_update::Content::Status(StatusMessage {
                                    message: format!("Loading {}...", nav.url),
                                    level: StatusLevel::Info.into(),
                                })),
                            }))
                            .await;

                        // Navigate
                        if let Some(page) = session_manager.get_page(&sid).await {
                            match BrowserPool::navigate(&page, &nav.url).await {
                                Ok(html) => {
                                    session_manager
                                        .set_current_url(&sid, nav.url.clone())
                                        .await;

                                    // Extract and convert
                                    let result =
                                        ExtractionPipeline::process(&html, &nav.url);

                                    let link_map: std::collections::HashMap<u32, String> =
                                        result.link_map;

                                    let _ = tx
                                        .send(Ok(PageUpdate {
                                            content: Some(
                                                page_update::Content::FullPage(
                                                    PreRenderedPage {
                                                        markdown: result.markdown,
                                                        link_map,
                                                        title: result.title,
                                                        url: nav.url,
                                                    },
                                                ),
                                            ),
                                        }))
                                        .await;
                                }
                                Err(e) => {
                                    let _ = tx
                                        .send(Ok(PageUpdate {
                                            content: Some(
                                                page_update::Content::Status(
                                                    StatusMessage {
                                                        message: format!(
                                                            "Navigation failed: {}",
                                                            e
                                                        ),
                                                        level: StatusLevel::Error
                                                            .into(),
                                                    },
                                                ),
                                            ),
                                        }))
                                        .await;
                                }
                            }
                        }
                    }

                    interaction::Type::Click(_click) => {
                        if let Some(ref sid) = session_id {
                            if let Some(_page) = session_manager.get_page(sid).await {
                                // Resolve link_id to URL — for now navigate directly
                                // The client sends the link_id, we'd need the link_map
                                // For simplicity, we store link_maps per session
                                let _ = tx
                                    .send(Ok(PageUpdate {
                                        content: Some(page_update::Content::Status(
                                            StatusMessage {
                                                message: "Processing click..."
                                                    .to_string(),
                                                level: StatusLevel::Info.into(),
                                            },
                                        )),
                                    }))
                                    .await;
                            }
                        }
                    }

                    interaction::Type::Input(input_req) => {
                        if let Some(ref sid) = session_id {
                            if let Some(page) = session_manager.get_page(sid).await {
                                if input_req.is_submit {
                                    let _ = InputDispatcher::submit(&page).await;
                                } else {
                                    let _ = InputDispatcher::type_text(
                                        &page,
                                        &input_req.text,
                                    )
                                    .await;
                                }
                            }
                        }
                    }
                }
            }

            // Cleanup session on stream close
            if let Some(sid) = session_id {
                tracing::info!("Cleaning up session: {}", sid);
                session_manager.remove_session(&sid).await;
            }
        });

        Ok(Response::new(Box::pin(ReceiverStream::new(rx))))
    }
}
