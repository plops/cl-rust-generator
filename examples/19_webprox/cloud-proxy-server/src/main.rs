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

    /// Load all resources (don't block images, CSS, fonts, etc.)
    #[arg(long, default_value_t = false)]
    load_all: bool,

    /// Load images
    #[arg(long, default_value_t = false)]
    load_images: bool,

    /// Load media (audio, video)
    #[arg(long, default_value_t = false)]
    load_media: bool,

    /// Load CSS
    #[arg(long, default_value_t = false)]
    load_css: bool,

    /// Load fonts
    #[arg(long, default_value_t = false)]
    load_fonts: bool,
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

    let flags = cloud_proxy_server::service::ResourceFlags {
        load_images: args.load_images,
        load_media: args.load_media,
        load_css: args.load_css,
        load_fonts: args.load_fonts,
        load_all: args.load_all,
    };

    let backend = BrowserBackend::new(session_manager, !args.no_headless, flags).await?;
    let svc = BrowsingServiceServer::new(backend);

    Server::builder()
        .add_service(svc)
        .serve(addr)
        .await?;

    Ok(())
}
