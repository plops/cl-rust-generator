use std::fs::File;
use std::io::prelude::*;
use std::thread::spawn;
use std::sync::mpsc::channel;
fn start_file_reader_thread (documents: Vec<PathBuf>) -> (Receiver<String>, JoinHandle<io::Result<()>>){
            let (sender, receiver)  = channel();
    let handle  = spawn(move ||{
                for  filename in documents {
                                    let mut f  = File::open(filename)?;
            let mut text  = String::new();
            f.read_to_string(&mut text)?;
            if  sender.send(text).is_err()  {
                                                break;
};
}
                return Ok(());
});
        return (receiver, handle);
}
fn start_file_indexing_thread (texts: Receiver<String>) -> (Receiver<InMemoryIndex>, JoinHandle<()>){
            let (sender, receiver)  = channel();
    let handle  = spawn(move ||{
                for  (doc_id, text) in texts.into_iter().enumerate() {
                                    let index  = InMemoryIndex::from_single_document(doc_id, text);
            if  sender.send(index).is_err()  {
                                break;
};
}
});
    return (receiver, handle);
}
for  text in receiver {
}
fn start_in_memory_merge_thread (file_indexes: Receiver<InMemoryIndex>) -> (Receiver<InMemoryIndex>, JoinHandle<()>){
}
fn start_index_writer_thread (big_indexes: Receiver<InMemoryIndex>, output_dir: &Path) -> (Receiver<PathBuf>, JoinHandle<io::Result<()>>){
}
fn merge_index_files (files: Receiver<PathBuf>, output_dir: &Path) -> io::Result<()>{
}