use cloud_proxy_server::service::BrowserBackend;
use cloud_proxy_server::session::SessionManager;

use proto_definitions::browser::browsing_service_server::BrowsingServiceServer;
use tonic::transport::Server;

#[tokio::main]
async fn main() -> Result<(), Box<dyn std::error::Error + Send + Sync>> {
    tracing_subscriber::fmt::init();

    let addr = "[::1]:50051".parse()?;
    tracing::info!("Starting cloud-proxy-server on {}", addr);

    let session_manager = SessionManager::new();

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
