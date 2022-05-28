# Issue Tracker
Keeping tabs on all the bugs

- [ ] Constant propagation doesn't work for negative numbers (and arithmetic in general lol)
- [ ] Nop instruction not created for empty blocks (especially w/ dead code elim)
- [ ] Call to OutputNum unsafely assumes correct number of args, panics otherwise
- [ ] Despite dead code elim, extra branch instruction still produced in then branch of if statements
- [ ] Integer literals should be stored as signed rather than unsigned
- [ ] A bunch of unused constants may still be allocated due to instruction selection
- [ ] Register allocation doesn't understand calling conventions
- [ ] Update simple register allocator for new location api
- [ ] Extra returns getting generated all over the place
- [ ] Const branching doesn't work for loops
- [ ] Simple register allocator doesn't properly respect calling conventions
- [ ] Calls in IR always allocate a result value, even if the function is void
- [ ] Functions not generating return instruction by default
