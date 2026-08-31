use clap::Args;
use std::path::PathBuf;

// NOTE: the CLI is defined once, in main.rs.  This module used to carry a second
// `ClientCli' struct, which made `parse_integration_test_args' unusable from
// main.rs (two distinct types with the same name).

#[derive(Args, Debug)]
pub struct IntegrationTestArgs {
    // fields here
    pub test_image: PathBuf,
    pub server_addr: String,
    pub tolerance: f32,
    pub output_result: Option<PathBuf>,
}

pub fn parse_integration_test_args(cli: crate::ClientCli) -> Option<IntegrationTestArgs> {
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
