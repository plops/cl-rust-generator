mod browser;
mod compressor;
mod differ;
mod extractor;
mod input;
mod service;
mod session;
mod throttle;

use service::BrowserBackend;
use session::SessionManager;
use tracing_subscriber;

use proto_definitions::browser::browsing_service_server::BrowsingServiceServer;
use tonic::transport::Server;

#[tokio::main]
async fn main() -> Result<(), Box<dyn std::error::Error + Send + Sync>> {
    tracing_subscriber::fmt::init();

    let addr = "[::1]:50051".parse()?;
    tracing::info!("Starting cloud-proxy-server on {}", addr);

    let session_manager = SessionManager::new();

    // Spawn the idle session reaper
    let reaper_sessions = session_manager.clone();
    tokio::spawn(async move {
        reaper_sessions.reap_idle_sessions_loop().await;
    });

    let backend = BrowserBackend::new(session_manager).await?;
    let svc = BrowsingServiceServer::new(backend);

    Server::builder()
        .add_service(svc)
        .serve(addr)
        .await?;

    Ok(())
}
