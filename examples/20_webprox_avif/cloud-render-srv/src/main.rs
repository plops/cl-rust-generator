use clap::Parser;
use log::{info, error};

mod server;
mod video;

#[derive(Parser, Debug)]
#[command(author, version, about = "AV1 Remote Browser - Cloud Render Server")]
struct Cli {
    #[arg(long, default_value = "127.0.0.1:50051")]
    addr: String,

    #[arg(long)]
    headless: bool,
}

#[tokio::main]
async fn main() -> Result<(), Box<dyn std::error::Error>> {
    env_logger::init_from_env(env_logger::Env::default().default_filter_or("info"));
    let cli = Cli::parse();

    info!("Starting Cloud Render Server on {}", cli.addr);

    let addr = cli.addr.parse()?;
    let server = server::RemoteBrowserServer::new();

    tonic::transport::Server::builder()
        .add_service(proto_def::RemoteBrowserServer::new(server))
        .serve(addr)
        .await?;

    Ok(())
}
