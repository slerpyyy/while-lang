# while-lang
*An interpreter for the WHILE programming language*

WHILE is an educational programming designed to consist of as little instructions as possible while still being Turing complete.

This interpreter supports the WHILE and FOR language as well as some language extensions including functions and tuples.
Utilities for compiling these language extensions away are currently experimental.

```
# This program multiplies two numbers

A := 3  { first input }
B := 4  { second input }

x0 := 0
One := 1
while A /= 0 do
    x0 := x0 + B
    A := A - One
od
```

| :warning: | **Note to students:** Please keep in mind that you won't have access to this tool during the exam | 
|-|-|

## Usage

When run with the `help` subcommand or the `-h` or `--help` flag, the interpreter outputs the following:
```
while-lang 0.1.0

An interpreter for the WHILE programming language

USAGE:
    while-lang.exe <SUBCOMMAND>

FLAGS:
    -h, --help       Print help information
    -V, --version    Print version information

SUBCOMMANDS:
    check      Check if a given program is well-formed without running it
    debug      Evaluate a program one instruction at a time
    help       Print this message or the help of the given subcommand(s)
    rewrite    Translate a program into a pure WHILE program (experimental)
    run        Run a given program
```

## Setup

To build this project from source, you will need a Rust compiler and the Cargo package manager.
We highly recommend installing `rustup` which takes care of installing and updating the entire Rust toolchain.

Checkout the [Getting Started](https://www.rust-lang.org/learn/get-started) section on the rust-lang website for more.

```sh
# clone the repo
git clone https://github.com/slerpyyy/while-lang.git
cd while-lang

# compile and run the help subcommand
cargo run -- help

# install the interpreter so you can run it from anywhere
cargo install --path .
```

## License

This project is licensed under either of

 * Apache License, Version 2.0
 * MIT license

at your option.

See the [license](LICENSE) file for details.

## Contribution

Unless you explicitly state otherwise, any contribution intentionally submitted
for inclusion in the work by you, as defined in the Apache-2.0 license, shall be
dual licensed as above, without any additional terms or conditions.
