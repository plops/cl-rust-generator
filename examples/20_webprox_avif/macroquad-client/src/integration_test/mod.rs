pub mod cli;
pub mod comparison;
pub mod image_loader;
pub mod reporter;
pub mod test_runner;

pub use cli::parse_integration_test_args;
pub use comparison::{compare_images, ComparisonResult};
pub use image_loader::load_test_image;
pub use reporter::{report_results, TestOutcome};
pub use test_runner::run_integration_test;
