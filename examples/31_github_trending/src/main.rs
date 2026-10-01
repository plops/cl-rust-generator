//! CLI-Einstiegspunkt: ausschließlich Verdrahtung (Args, stdin/Datei, Exit-Codes).

use std::fs;
use std::io::{self, Read};
use std::path::{Path, PathBuf};
use std::process::ExitCode;

use github_trending_algos::{config, mcp_client, output, parser, pipeline};

fn main() -> ExitCode {
    let args = match config::parse_args(std::env::args().skip(1)) {
        Ok(config::ArgsOutcome::Help) => {
            let help = config::usage();
            println!("{help}");
            return ExitCode::SUCCESS;
        }
        Ok(config::ArgsOutcome::Run(args)) => args,
        Err(message) => {
            eprintln!("{message}");
            return ExitCode::from(2);
        }
    };
    let verbose = args.verbose;

    if verbose {
        match args.input_file.as_ref() {
            Some(file) => eprintln!("Eingabe: Datei '{}'", file.display()),
            None => eprintln!("Eingabe: stdin"),
        }
    }
    let text = match read_input(args.input_file.as_ref()) {
        Ok(text) => text,
        Err(message) => {
            eprintln!("Fehler: {message}");
            return ExitCode::FAILURE;
        }
    };
    if verbose {
        let lines = text.lines().count();
        let unit = if lines == 1 { "Zeile" } else { "Zeilen" };
        eprintln!("Eingabe: {} Bytes, {lines} {unit} gelesen", text.len());
    }

    let repos = parser::parse_repos(&text);
    if verbose {
        let unit = if repos.len() == 1 {
            "Repository"
        } else {
            "Repositories"
        };
        eprintln!("Parse: {} {unit} gefunden", repos.len());
        for (index, repo) in repos.iter().enumerate() {
            eprintln!(
                "  [parse {}/{}] {} (Organisation='{}', Projekt='{}')",
                index + 1,
                repos.len(),
                repo.full_name(),
                repo.owner(),
                repo.name()
            );
        }
    }
    if repos.is_empty() {
        eprintln!("Keine Repositories im Format 'owner/repo' gefunden.");
        return ExitCode::FAILURE;
    }

    let settings = config::Config {
        delay: args.delay,
        ..config::Config::default()
    };
    let mut progress = io::stderr();
    if verbose {
        eprintln!(
            "MCP: initialisiere {} (protocolVersion={})",
            settings.endpoint, settings.protocol_version
        );
    }
    let mut client = mcp_client::McpClient::new(&settings);
    if let Err(error) = client.initialize(&mut progress, verbose) {
        eprintln!("Warnung: MCP-Initialize fehlgeschlagen ({error}); versuche es trotzdem.");
    }

    let report = pipeline::run(
        &repos,
        &mut client,
        &pipeline::ThreadSleeper,
        settings.delay,
        &mut progress,
        verbose,
    );

    if verbose {
        eprintln!(
            "Ausgabe: schreibe Reports ({} OK, {} fehlend, {} Fehler)",
            report.successes().len(),
            report.missing().len(),
            report.failed().len()
        );
    }
    match output::write_reports(&report, Path::new(".")) {
        Ok((algos_path, missing_path)) => {
            println!(
                "Fertig: {} OK, {} nicht indiziert, {} Fehler.",
                report.successes().len(),
                report.missing().len(),
                report.failed().len()
            );
            println!(
                "Dateien: {} und {}",
                algos_path.display(),
                missing_path.display()
            );
            ExitCode::SUCCESS
        }
        Err(error) => {
            eprintln!("Fehler beim Schreiben der Reports: {error}");
            ExitCode::FAILURE
        }
    }
}

/// Liest den Trending-Text aus einer Datei oder (ohne Pfad) von `stdin`.
fn read_input(path: Option<&PathBuf>) -> Result<String, String> {
    match path {
        Some(file) => fs::read_to_string(file)
            .map_err(|error| format!("Datei '{}' nicht lesbar: {error}", file.display())),
        None => {
            let mut buffer = String::new();
            io::stdin()
                .read_to_string(&mut buffer)
                .map_err(|error| format!("stdin nicht lesbar: {error}"))?;
            Ok(buffer)
        }
    }
}
