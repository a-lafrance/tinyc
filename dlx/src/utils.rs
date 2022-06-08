use std::io::BufRead;
use crate::{isa::Instruction, LoadError};

pub fn read_instr(reader: &mut impl BufRead, buf: &mut [u8; 4]) -> Result<Option<Instruction>, LoadError> {
    if reader.read_exact(buf).is_err() {
        return Ok(None); // is this valid? rust in action says so
    }

    let bytes = u32::from_be_bytes(*buf);
    Instruction::try_from(bytes).map(Some).map_err(|e| e.into())
}

pub fn word_to_byte_addr(w: usize) -> usize {
    w * 4
}

pub fn byte_to_word_addr(b: usize) -> Option<usize> {
    // is there a better way to do this?
    if b % 4 == 0 {
        Some(b / 4)
    } else {
        None
    }
}
