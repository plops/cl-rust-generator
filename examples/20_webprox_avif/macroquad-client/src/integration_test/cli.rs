use clap::{Args, Parser};
use std::path::PathBuf;

#[derive(Parser, Debug)]
#[command(author, version, about = "AV1 Remote Browser Client")]
pub struct ClientCli {
    /// Enable integration test mode
    #[arg(long)]
    pub integration_test: bool,
    
    /// Path to test PNG image
    #[arg(long)]
    pub test_image: Option<PathBuf>,
    
    /// Server address (default: [::1]:50051)
    #[arg(long, default_value = "[::1]:50051")]
    pub server_addr: String,
    
    /// Pixel comparison tolerance (0.0 = exact match)
    #[arg(long, default_value = "0.0")]
    pub tolerance: f32,
    
    /// Path to save comparison result JSON
    #[arg(long)]
    pub output_result: Option<PathBuf>,
    
    /// Manual Y offset adjustment for debugging coordinate alignment (in pixels)
    #[arg(long, default_value = "0")]
    pub y_offset: i32,
    
    /// Enable verbose coordinate logging
    #[arg(long)]
    pub verbose_coords: bool,
}

#[derive(Args, Debug)]
pub struct IntegrationTestArgs {
    pub test_image: PathBuf,
    pub server_addr: String,
    pub tolerance: f32,
    pub output_result: Option<PathBuf>,
}

pub fn parse_integration_test_args(cli: ClientCli) -> Option<IntegrationTestArgs> {
    if !cli.integration_test {
        return None;
    }
    
    let test_image = cli.test_image?;
    
    Some(IntegrationTestArgs {
        test_image,
        server_addr: cli.server_addr,
        tolerance: cli.tolerance,
        output_result: cli.output_result,
    })
}
