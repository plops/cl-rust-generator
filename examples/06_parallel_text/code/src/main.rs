#[allow(unused_parens)]
extern crate argparse;
extern crate byteorder;
mod index;
use argparse::{ArgumentParser, Collect, StoreTrue};
use index::InMemoryIndex;
use std::error::Error;
use std::fs::File;
use std::io;
use std::io::prelude::*;
use std::path::{Path, PathBuf};
use std::sync::mpsc::{channel, Receiver};
use std::thread::{spawn, JoinHandle};
fn start_file_reader_thread(
    documents: Vec<PathBuf>,
) -> (Receiver<String>, JoinHandle<io::Result<()>>) {
    let (sender, receiver) = channel();
    let handle = spawn(move || {
        for filename in documents {
            let mut f = File::open(filename)?;
            let mut text = String::new();
            f.read_to_string(&mut text)?;
            if sender.send(text).is_err() {
                break;
            };
        }
        return Ok(());
    });
    return (receiver, handle);
}
fn start_file_indexing_thread(
    texts: Receiver<String>,
) -> (Receiver<InMemoryIndex>, JoinHandle<()>) {
    let (sender, receiver) = channel();
    let handle = spawn(move || {
        for (doc_id, text) in texts.into_iter().enumerate() {
            let index = InMemoryIndex::from_single_document(doc_id, text);
            if sender.send(index).is_err() {
                break;
            };
        }
    });
    return (receiver, handle);
}
fn start_in_memory_merge_thread(
    file_indexes: Receiver<InMemoryIndex>,
) -> (Receiver<InMemoryIndex>, JoinHandle<()>) {
    let (sender, receiver) = channel();
    let handle = spawn(move || {
        let mut accumulated_index = InMemoryIndex::new();
        for fi in file_indexes {
            accumulated_index.merge(fi);
            if accumulated_index.is_large() {
                if sender.send(accumulated_index).is_err() {
                    return;
                };
                accumulated_index = InMemoryIndex::new();
            };
        }
        if !(accumulated_index.is_empty()) {
            let _ = sender.send(accumulated_index);
        };
    });
    return (receiver, handle);
}
fn run_pipeline(documents: Vec<PathBuf>, output_dir: PathBuf) -> io::Result<()> {
    let (texts, h1) = start_file_reader_thread(documents);
    let (pints, h2) = start_file_indexing_thread(texts);
    let r1 = h1.join().unwrap();
    return Ok(());
}
fn expand_filename_arguments(args: Vec<String>) -> io::Result<Vec<PathBuf>> {
    let mut filenames = vec![];
    for arg in args {
        let path = PathBuf::from(arg);
        if path.metadata()?.is_dir() {
            for entry in path.read_dir()? {
                let entry = entry?;
                if entry.file_type()?.is_file() {
                    filenames.push(entry.path());
                };
            }
        } else {
            filenames.push(path);
        };
    }
    return Ok(filenames);
}
fn run(filenames: Vec<String>) -> io::Result<()> {
    let output_dir = PathBuf::from(".");
    let documents = expand_filename_arguments(filenames)?;
    return run_pipeline(documents, output_dir);
}
fn main() {
    let mut filenames = vec![];
    {
        let mut ap = ArgumentParser::new();
        ap.set_description("make inverted index for searching documents");
        ap.refer(&mut filenames)
            .add_argument("filenames", Collect, "files/directories to index");
        ap.parse_args_or_exit();
    };
    match (run(filenames)) {
        Ok(()) => return {},
        Err(err) => println!("error: {:?}", err.description()),
    };
}
