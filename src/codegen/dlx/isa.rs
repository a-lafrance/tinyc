pub enum Instruction {
    F1(F1Opcode, Register, Register, u32),
    F2(F2Opcode, Register, Register, Register),
    F3(F3Opcode, u32),
}

pub enum F1Opcode {
    Addi = 16, Subi, Muli, Divi, Cmpi = 21,
    Ldw = 32, Pop = 34, Stw = 36, Psh = 38,
    Beq = 40, Bne, Blt, Bge, Ble, Bgt, Wrl = 53
}

pub enum F2Opcode {
    Add, Sub, Mul, Div, Cmp = 5, Ldx = 33, Stx = 37, Rdd = 50, Wrd,
}

pub enum F3Opcode {
    Addi = 16, Subi, Muli, Divi, Cmpi = 21,
}

// enum?
pub struct Register(u8);
