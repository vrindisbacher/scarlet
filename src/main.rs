// And example of what I need:
//
// Global type context
//

use anyhow::{Context, Result};
use clap::Parser;
use std::{
    path::{Path, PathBuf},
    str::FromStr,
};
use walkdir::WalkDir;

use crate::r#gen::CodeGenerator;

mod ast;
mod r#gen;
mod parser;

#[derive(Clone, Copy)]
enum RequestLanguages {
    Typescript,
}

impl FromStr for RequestLanguages {
    type Err = anyhow::Error;

    fn from_str(s: &str) -> std::result::Result<Self, Self::Err> {
        match s {
            "typescript" => Ok(Self::Typescript),
            _ => Err(anyhow::format_err!(
                "{} is not a supported language for requests.",
                s
            )),
        }
    }
}

#[derive(Clone, Copy)]
enum ResponseLanguages {
    Rust,
    Typescript,
}

impl FromStr for ResponseLanguages {
    type Err = anyhow::Error;

    fn from_str(s: &str) -> std::result::Result<Self, Self::Err> {
        match s {
            "typescript" => Ok(Self::Typescript),
            "rust" => Ok(Self::Rust),
            _ => Err(anyhow::format_err!(
                "{} is not a supported language for response.",
                s
            )),
        }
    }
}

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
        process_scarlet_file(file_path, &cli)?;
    }

    Ok(())
}

fn process_scarlet_file(file_path: &Path, cli: &Cli) -> Result<()> {
    let request_language = RequestLanguages::from_str(&cli.request_language)?;
    let response_language = ResponseLanguages::from_str(&cli.response_language)?;

    // Read the file
    let content = std::fs::read_to_string(file_path)
        .with_context(|| format!("Failed to read file: {}", file_path.display()))?;

    // Parse the file
    match parser::parse_scarlet_file(&content) {
        Ok(ast) => {
            let code_gen = CodeGenerator::new(request_language, response_language);
            let res = code_gen.gen_from(ast)?;

            println!("REQUEST");
            println!("{}", res.request());

            println!("RESPONSE");
            println!("{}", res.response());
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
