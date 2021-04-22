# Haskell2

## Basic Requirements
- [x] Quit command
- [x] Tracking system State
- [x] Extend parser to support multi-digit numbers and whitespace around operators
- [x] String concatenation
- [x] Implement conversion between string to int functionality
- [x] Implement command to read user input as string functionality
  - [ ] Input has some bugs (print input)

## Easy Requirements
- [ ] Implement Quickcheck
  - [ ] Try migrating to stack to sort out all the damned cabal library errors and so I can just run the test suite without opening ghci
  - [ ] Try creating better generators: a string generator that just creates 
  - [ ] Float bug: can't parse floats in the form "1.0e-2"
  - [ ] String bug: can't parse backslash \
  - [ ] Concat bug: enters infinite loop when concatenating strings
- [x] Implement functionality for abs, mod, and power
- [x] Extend parser to support negative numbers
- [x] Support floats

## Medium Requirements
- [x] Implement binary search tree
- [ ] Add command to read and process input files with commands
- [ ] Implement better treatment of errors using Either type
- [x] Implement functionality for if...then...else

## Hard Requirements
- [x] Add Haskeline for command history
  - [ ] Implement tab completion for commands and variable names
- [ ] Add command for simple repetition
- [x] Implement functionality for loop constructs
- [ ] Allow defining and calling functions
