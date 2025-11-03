use std::io;
use std::io::Read;

pub struct ConcatReader<A, B> {
    a: A,
    b: B,
}

impl<A, B> ConcatReader<A, B> {
    pub fn new(a: A, b: B) -> Self {
        Self {
            a, 
            b,
        }
    }
}

impl<A, B> Read for ConcatReader<A, B> 
    where A: Read, B: Read
{
    fn read(&mut self, buf: &mut [u8]) -> io::Result<usize> {
        let from_a = self.a.read(buf)?;
        
        if from_a == buf.len() {
            return Ok(from_a);
        }
        
        let from_b = self.b.read(&mut buf[from_a..])?;
        Ok(from_a + from_b)
    }
}

#[cfg(test)]
mod test {
    use crate::concat_reader::ConcatReader;
    use std::io::Read;

    #[test]
    pub fn reads_simple() {
        let a = "hello, ";
        let b = "world!";
        
        let mut reader = ConcatReader::new(a.as_bytes(), b.as_bytes());
        
        const EXPECT: &str = "hello, world!";
        let mut buf = [0u8; EXPECT.len()];
        
        reader.read_exact(&mut buf).unwrap();
        
        assert_eq!(EXPECT, String::from_utf8(buf.to_vec()).unwrap());
    }
}
