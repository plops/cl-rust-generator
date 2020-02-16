use std::fs::File;
use std::io::prelude::*;
use std::thread::spawn;
use std::sync::mpsc::channel;
;
let (sender, receiver)  = channel();
let handle  = spawn(move ||{
        for  filename in documents {
                        let mut f  = File::open(filename)?;
        let mut text  = String::new();
        f.read_to_string(&mut text)?;
        if ( sender.send(text).is_err() ) {
                                    break;
};
}
        return Ok(());
});