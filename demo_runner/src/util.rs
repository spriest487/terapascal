use std::io;
use std::io::Read;

pub fn read_to_line_feed(mut src: impl Read, buf: &mut Vec<u8>) -> io::Result<String> {
    loop {
        let mut next = [0];
        src.read_exact(&mut next)?;

        if next[0] == b'\n' {
            break buf_to_string(buf.clone());
        }

        if next[0] != b'\r' {
            buf.push(next[0]);
        }
    }
}

pub fn buf_to_string(buf: Vec<u8>) -> io::Result<String> {
    match String::from_utf8(buf) {
        Ok(string) => Ok(string.trim_end().to_string()),
        Err(err) => {
            let msg = format!("unreadable output: {}", err);
            Err(io::Error::new(io::ErrorKind::InvalidData, msg))
        }
    }
}