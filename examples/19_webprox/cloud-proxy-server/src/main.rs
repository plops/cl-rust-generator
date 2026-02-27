use cloud_proxy_server::service::BrowserBackend;
use cloud_proxy_server::session::SessionManager;

use proto_definitions::browser::browsing_service_server::BrowsingServiceServer;
use tonic::transport::Server;
use clap::Parser;

#[derive(Parser, Debug)]
#[command(name = "cloud-proxy-server")]
#[command(about = "Remote browser proxy server", long_about = None)]
struct Args {
    /// Disable headless mode (show browser window)
    #[arg(long, default_value_t = false)]
    no_headless: bool,
}

#[tokio::main]
async fn main() -> Result<(), Box<dyn std::error::Error + Send + Sync>> {
    tracing_subscriber::fmt::init();

    let args = Args::parse();

    let addr = "[::1]:50051".parse()?;
    tracing::info!("Starting cloud-proxy-server on {}", addr);
    if args.no_headless {
        tracing::info!("Running in visible mode (headless disabled)");
    }

    let session_manager = SessionManager::new();

    let reaper_sessions = session_manager.clone();
    tokio::spawn(async move {
        reaper_sessions.reap_idle_sessions_loop().await;
    });

    let backend = BrowserBackend::new(session_manager, !args.no_headless).await?;
    let svc = BrowsingServiceServer::new(backend);

    Server::builder()
        .add_service(svc)
        .serve(addr)
        .await?;

    Ok(())
}
