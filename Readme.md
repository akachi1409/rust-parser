# Getting Started with Arithmetic Parser 

This is a parser to take a string and compute its numerical value using precedence climbing algorithm.
Operators are applied in order of precedence from left to right. An exception to this is brackets which are used to explicitly denote precedence by grouping parts of an expression that should be evaluated first.

Rules
a = ‘+’, b = ‘-’, c = ‘*’, d = ‘/’, e = ‘(’, f = ‘)’

## Running in Local Environment

This project is created by rust.

- Install rustup.exe on windows
- Setup environment path for rustc and cargo
- Run `cargo run`

## Creating a Production Build

- Run `cargo build`

## Sample input

- Input: “3a2c4”    Result: 20
- Input: “32a2d2"   Result: 17
- Input: “500a10b66c32”     Result: 14208
- Input: “3ae4c66fb32"      Result: 235
- Input: “3c4d2aee2a4c41fc4f”   Result: 990



