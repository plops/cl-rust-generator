#[allow(unused_parens)]
use chrono::{DateTime, Utc};
extern crate argparse;
extern crate byteorder;
extern crate chardet;
extern crate chrono;
extern crate encoding;
mod index;
mod merge;
mod read;
mod tmp;
mod write;
use argparse::{ArgumentParser, Collect};
use chardet::{charset2encoding, detect};
use encoding::label::encoding_from_whatwg_label;
use encoding::DecoderTrap;
use index::InMemoryIndex;
use merge::FileMerge;
use std::error::Error;
use std::fs::File;
use std::fs::OpenOptions;
use std::io;
use std::io::prelude::*;
use std::path::{Path, PathBuf};
use std::sync::mpsc::{channel, sync_channel, Receiver};
use std::thread::{spawn, JoinHandle};
use tmp::TmpDir;
use write::write_index_to_tmp_file;
fn start_file_reader_thread(
    documents: Vec<PathBuf>,
) -> (Receiver<String>, JoinHandle<io::Result<()>>) {
    let (sender, receiver) = sync_channel(32);
    let handle = spawn(move || {
        for filename in documents {
            let mut fh = OpenOptions::new()
                .read(true)
                .open(filename)
                .expect("could not open file");
            let mut reader: Vec<u8> = Vec::new();
            fh.read_to_end(&mut reader).expect("could not read file");
            let detected_charset = detect(&reader);
            let coder = encoding_from_whatwg_label(charset2encoding(&detected_charset.0));
            if coder.is_some() {
                let utf8reader = coder
                    .unwrap()
                    .decode(&reader, DecoderTrap::Ignore)
                    .expect("could not convert to utf:8");
                if sender.send(utf8reader).is_err() {
                    break;
                };
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
fn start_index_writer_thread(
    big_indexes: Receiver<InMemoryIndex>,
    output_dir: &Path,
) -> (Receiver<PathBuf>, JoinHandle<io::Result<()>>) {
    let (sender, receiver) = channel();
    let mut tmp_dir = TmpDir::new(output_dir);
    let handle = spawn(move || {
        for index in big_indexes {
            let file = write_index_to_tmp_file(index, &mut tmp_dir)?;
            if sender.send(file).is_err() {
                break;
            };
        }
        return Ok(());
    });
    return (receiver, handle);
}
fn merge_index_files(files: Receiver<PathBuf>, output_dir: &Path) -> io::Result<()> {
    let mut merge = FileMerge::new(output_dir);
    for file in files {
        {
            println!(
                "{} {}:{} merge index  file.display()={}",
                Utc::now(),
                file!(),
                line!(),
                file.display()
            );
        }
        merge.add_file(file)?;
    }
    return merge.finish();
}
fn run_pipeline(documents: Vec<PathBuf>, output_dir: PathBuf) -> io::Result<()> {
    {
        println!("{} {}:{} run_pipeline ", Utc::now(), file!(), line!());
    }
    let (texts, h1) = start_file_reader_thread(documents);
    let (pints, h2) = start_file_indexing_thread(texts);
    let (gallons, h3) = start_in_memory_merge_thread(pints);
    let (files, h4) = start_index_writer_thread(gallons, &output_dir);
    let result = merge_index_files(files, &output_dir);
    let r1 = h1.join().unwrap();
    h2.join().unwrap();
    h3.join().unwrap();
    let r4 = h4.join().unwrap();
    r1?;
    r4?;
    return result;
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
        println!("{} {}:{} start ", Utc::now(), file!(), line!());
    }
    {
        let mut ap = ArgumentParser::new();
        ap.set_description("make inverted index for searching documents");
        ap.refer(&mut filenames)
            .add_argument("filenames", Collect, "files/directories to index");
        ap.parse_args_or_exit();
    }
    match run(filenames) {
        Ok(()) => return {},
        Err(err) => println!("error: {:?}", err.description()),
    };
}
