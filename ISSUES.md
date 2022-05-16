# Issue Tracker
Keeping tabs on all the bugs

- [ ] Constant propagation doesn't work for negative numbers
- [ ] Nop instruction not created for empty blocks (especially w/ dead code elim)
- [ ] Call to OutputNum unsafely assumes correct number of args, panics otherwise
- [ ] Despite dead code elim, extra branch instruction still produced in then branch of if statements
- [ ] Integer literals should be stored as signed rather than unsigned
- [ ] A bunch of unused constants may still be allocated due to instruction selection
