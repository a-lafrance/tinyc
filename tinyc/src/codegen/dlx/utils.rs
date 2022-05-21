macro_rules! do_with_store {
    ($self:ident, $val:ident => $reg:ident, $body:block) => {
        // get the reg
        // emit the body
        // emit the store
        let ($reg, offset) = match $self.loc_for_val($val) {
            Location::Reg(r) => (r, None),
            Location::Stack(offset) => (Self::STACK_TMP1, Some(offset)),
        };

        $body

        if let Some(offset) = offset {
            $self.emit_store($reg, Register::RSP, $self.stack_offset(offset as i16));
        }
    }
}
