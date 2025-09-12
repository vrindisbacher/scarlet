// And example of what I need:
//
// Global type context
//

use anyhow::{Context, Result};
use clap::Parser;
use std::path::{Path, PathBuf};
use walkdir::WalkDir;

mod ast;
mod parser;

#[derive(Parser)]
#[command(name = "scarlet")]
#[command(about = "A DSL for defining API types and services")]
#[command(version = "0.1.0")]
struct Cli {
    /// Directory to scan for .gd files
    #[arg(short, long, default_value = ".")]
    directory: PathBuf,

    /// Output directory for generated files
    #[arg(short, long, default_value = "./generated")]
    output: PathBuf,

    /// Target languages for code generation
    #[arg(short = 'I', long, default_value = "typescript")]
    request_language: String,

    #[arg(short = 'O', long, default_value = "rust")]
    response_language: String,
}

fn main() -> Result<()> {
    let cli = Cli::parse();

    if !cli.directory.exists() {
        anyhow::bail!("Directory {:?} does not exist", cli.directory);
    }

    let scarlet_files = find_scarlet_files(&cli.directory, "gd")?;

    if scarlet_files.is_empty() {
        println!("No scarlet files found in {:?}", cli.directory);
        return Ok(());
    }

    for file_path in scarlet_files.iter() {
        process_scarlet_file(file_path)?;
    }

    Ok(())
}

fn process_scarlet_file(file_path: &Path) -> Result<()> {
    // Read the file
    let content = std::fs::read_to_string(file_path)
        .with_context(|| format!("Failed to read file: {}", file_path.display()))?;

    // Parse the file
    match parser::parse_scarlet_file(&content) {
        Ok(ast) => {
            println!("{ast:?}");
        }
        Err(e) => {
            eprintln!("Parse error in {}: {}", file_path.display(), e);
        }
    }

    Ok(())
}

fn find_scarlet_files(directory: &Path, extension: &str) -> anyhow::Result<Vec<PathBuf>> {
    let mut scarlet_files = Vec::new();

    for entry in WalkDir::new(directory) {
        let entry = entry.context("Failed to read directory entry")?;
        let path = entry.path();

        if path.is_file() {
            if let Some(ext) = path.extension() {
                if ext == extension {
                    scarlet_files.push(path.to_path_buf());
                }
            }
        }
    }

    Ok(scarlet_files)
}
