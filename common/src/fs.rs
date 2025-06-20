use std::borrow::Cow;
use std::fs::File;
use std::io;
use std::io::Read;
use std::io::Seek;
use std::io::SeekFrom;
use std::path::PathBuf;
use encoding_rs::{Encoding, UTF_8};

pub trait Filesystem {
    fn read_source(&self, path: &PathBuf) -> io::Result<Cow<str>>;
    
    fn exists(&self, path: &PathBuf) -> bool;
    fn is_dir(&self, path: &PathBuf) -> bool;
    
    fn canonicalize(&self, path: &PathBuf) -> io::Result<PathBuf>;
}

pub struct DefaultFilesystem;

impl Filesystem for DefaultFilesystem {
    fn read_source(&self, path: &PathBuf) -> io::Result<Cow<str>> {
        let mut file = File::open(path)?;
        
        let end_pos = file.seek(SeekFrom::End(0))?;

        let Ok(len) = usize::try_from(end_pos) else {
            return Err(io::Error::new(
                io::ErrorKind::FileTooLarge, 
                format!("{} is too large ({end_pos})", path.display())
            ));
        };
        
        file.seek(SeekFrom::Start(0))?;

        let mut file_buf = Vec::with_capacity(len);
        file.read_to_end(&mut file_buf)?;

        let (encoding, _bom_len) = Encoding::for_bom(&file_buf).unwrap_or((UTF_8, 3));

        let (src_str, _replaced) = encoding.decode_with_bom_removal(&file_buf);

        Ok(Cow::Owned(src_str.into_owned()))
    }

    fn exists(&self, path: &PathBuf) -> bool {
        path.exists()
    }

    fn is_dir(&self, path: &PathBuf) -> bool {
        path.is_dir()
    }

    fn canonicalize(&self, path: &PathBuf) -> io::Result<PathBuf> {
        path.canonicalize()
    }
}
