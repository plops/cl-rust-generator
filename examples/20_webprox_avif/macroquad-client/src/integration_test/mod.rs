pub mod cli;
pub mod test_runner;
pub mod image_loader;
pub mod comparison;
pub mod reporter;

pub use cli::{IntegrationTestArgs, parse_integration_test_args};
pub use test_runner::run_integration_test;
pub use image_loader::{load_test_image, TestImageData};
pub use comparison::{compare_images, ComparisonResult};
pub use reporter::{report_results, TestOutcome};
