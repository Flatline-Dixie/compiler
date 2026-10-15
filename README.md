# Arithmetic Expression Compiler

A small Haskell compiler that parses arithmetic expressions, compiles them to
stack-machine bytecode, and evaluates the resulting program.

## Requirements

- GHC (the Glasgow Haskell Compiler)
- `runghc` (usually installed with GHC)

## Run

Pass an expression as command-line arguments:

```sh
runghc main.hs "10 + (3 * 4)"
```

Or provide an expression on standard input:

```sh
echo "10 + (3 * 4)" | runghc main.hs
```

The program prints the compiled bytecode and the evaluated result:

```text
Bytecode: [Push 10,Push 3,Push 4,OpMul,OpAdd]
Result: 22
```

To compile an executable:

```sh
ghc -O2 main.hs -o arithmetic-compiler
./arithmetic-compiler "10 + (3 * 4)"
```

## Expression syntax

The compiler supports:

- Integer literals
- Parentheses
- Unary plus and minus (for example, `-5` or `-(2 + 3)`)
- Addition (`+`) and subtraction (`-`)
- Multiplication (`*`) and integer division (`/`)

Multiplication and division have higher precedence than addition and
subtraction. Operators of the same precedence are evaluated left to right.
Division uses Haskell's integer `div` operation.

Malformed expressions and division by zero are reported as errors, and cause
the program to exit unsuccessfully.
