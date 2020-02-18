use std::fs::{self, File};
use std::io::{self, BufWriter};
use std::path::{Path, PathBuf};
#[allow(unused_parens)]
use std::time::{SystemTime, UNIX_EPOCH};
#[derive(Clone)]
pub struct TmpDir {
    dir: PathBuf,
    n: usize,
}
impl TmpDir {
    pub fn new<P: AsRef<Path>>(dir: P) -> TmpDir {
        return TmpDir {
            dir: dir.as_ref().to_owned(),
            n: 1,
        };
    }
    pub fn create(&mut self) -> io::Result<(PathBuf, BufWriter<File>)> {
        let mut try_count = 1;
        loop {
            let filename = self
                .dir
                .join(PathBuf::from(format!("tmp{:08x}.dat", self.n)));
            self.n += 1;
            match fs::OpenOptions::new()
                .write(true)
                .create_new(true)
                .open(&filename)
            {
                Ok(f) => return Ok((filename, BufWriter::new(f))),
                Err(exc) => {
                    if !((try_count < 999) && ((io::ErrorKind::AlreadyExists) == (exc.kind()))) {
                        return Err(exc);
                    }
                }
            };
            try_count += 1;
        }
    }
}
