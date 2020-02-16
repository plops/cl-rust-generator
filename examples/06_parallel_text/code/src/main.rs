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
}
for  text in receiver {
}