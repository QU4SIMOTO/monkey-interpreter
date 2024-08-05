# Monkey Interpreter
Simple rust implementation of the interpreter outlined in [Writing An Interpreter In Go](https://interpreterbook.com).

## Description
This is a learning project and is optimised or particularly well implemented. Most of the features outlined in the book are working but errors aren't yet handled correctly

## Running the interpreter 

### Repl
```sh
cargo run
```
### Using file
```sh
cargo run -- <PATH>
```

## TODO
- [ ] Add line and column numbers to parsing errors
- [ ] Print out parsing and evaluation errors in repl instead of filtering them out
- [ ] Add integeration tests
- [ ] Refactor to avoid unnecessary allocations in Lexer and Parser
- [ ] Add rustdoc
