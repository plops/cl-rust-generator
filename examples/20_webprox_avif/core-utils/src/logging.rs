use log::{LevelFilter, SetLoggerError};
use env_logger::{Builder, Target};
use std::io::Write;

/// Initialize logging with consistent format for all components
pub fn init_logging(component_name: &str) -> Result<(), SetLoggerError> {
    let mut builder = Builder::from_default_env();
    
    builder
        .filter_level(LevelFilter::Debug)
        .format(|buf, record| {
            writeln!(
                buf,
                "{} [{}] [{}:{}] {}",
                chrono::Utc::now().format("%Y-%m-%d %H:%M:%S%.3f"),
                record.level(),
                record.target(),
                record.line().unwrap_or(0),
                record.args()
            )
        })
        .target(Target::Stdout)
        .init();
    
    log::info!("Logging initialized for component: {}", component_name);
    Ok(())
}

/// Initialize logging with custom log level
pub fn init_logging_with_level(component_name: &str, level: LevelFilter) -> Result<(), SetLoggerError> {
    let mut builder = Builder::from_default_env();
    
    builder
        .filter_level(level)
        .format(|buf, record| {
            writeln!(
                buf,
                "{} [{}] [{}:{}] {}",
                chrono::Utc::now().format("%Y-%m-%d %H:%M:%S%.3f"),
                record.level(),
                record.target(),
                record.line().unwrap_or(0),
                record.args()
            )
        })
        .target(Target::Stdout)
        .init();
    
    log::info!("Logging initialized for component: {} with level: {}", component_name, level);
    Ok(())
}
