use anyhow::Result;
use serde::{Deserialize, Serialize};
use std::path::Path;
use crate::integration_test::ComparisonResult;

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum TestOutcome {
    Passed,
    Failed(String),
}

#[derive(Debug, Serialize, Deserialize)]
pub struct TestReport {
    pub outcome: TestOutcome,
    pub comparison: Option<ComparisonResult>,
    pub test_image_path: String,
    pub server_addr: String,
    pub timestamp: String,
}

pub fn report_results(
    outcome: TestOutcome,
    comparison: Option<ComparisonResult>,
    test_image_path: &str,
    server_addr: &str,
    output_path: Option<&Path>,
) -> Result<()> {
    let report = TestReport {
        outcome: outcome.clone(),
        comparison,
        test_image_path: test_image_path.to_string(),
        server_addr: server_addr.to_string(),
        timestamp: chrono::Utc::now().to_rfc3339(),
    };
    
    // Print summary to console
    match &outcome {
        TestOutcome::Passed => {
            println!("[Client] ✅ Integration test PASSED");
        }
        TestOutcome::Failed(reason) => {
            println!("[Client] ❌ Integration test FAILED: {}", reason);
        }
    }
    
    // Save detailed report if requested
    if let Some(path) = output_path {
        let json = serde_json::to_string_pretty(&report)?;
        std::fs::write(path, json)?;
        println!("[Client] Detailed report saved to: {}", path.display());
    }
    
    // Exit with appropriate code
    match outcome {
        TestOutcome::Passed => std::process::exit(0),
        TestOutcome::Failed(_) => std::process::exit(1),
    }
}
