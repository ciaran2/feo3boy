# Integration Tests

This directory is for integration tests. Unlike normal tests, every file in this directory
gets compiled as a separate crate which depends on the library defined in `src`.

## Compiling

Binary output is stored in this directory in github in order to allow tests to be run on
machines which do not have `rgbasm` and `rgblink`. In order to re-build the actual code
used in the tests, you will need to install `rgbasm` and `rgblink`.

To run the tests just use `cargo test`. To rebuild the `.bin` files from the `.asm` files,
use `make`.
