use std::io::BufRead;
use crate::{isa::Instruction, LoadError};

pub fn read_instr(reader: &mut impl BufRead, buf: &mut [u8; 4]) -> Result<Option<Instruction>, LoadError> {
    if reader.read_exact(buf).is_err() {
        return Ok(None); // is this valid? rust in action says so
    }

    let bytes = u32::from_be_bytes(*buf);
    Instruction::try_from(bytes).map(|i| Some(i)).map_err(|e| e.into())
}
