use crate::ir::isa::BasicBlock;

macro_rules! do_with_store {
    ($self:ident, $val:ident => $reg:ident, $body:block) => {
        // get the reg
        if let Some(loc) = $self.loc_for_val($val) {
            let ($reg, offset) = match loc {
                Location::Reg(r) => (r, None),
                Location::Stack(offset) => (Self::STACK_TMP1, Some(offset)),
            };

            // emit the body
            $body

            // emit the store
            if let Some(offset) = offset {
                $self.emit_store($reg, Register::RFP, $self.stack_offset(offset));
            }
        }
    }
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct UnresolvedBranch {
    pub ip: usize,
    pub addr: usize,
    pub dest: BasicBlock,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct UnresolvedCall {
    pub ip: usize,
    pub dest: String,
}
