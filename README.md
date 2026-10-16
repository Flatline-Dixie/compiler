# Arithmetic Expression Compiler

A small Haskell compiler that parses arithmetic expressions, folds constants,
compiles them to stack-machine bytecode, and evaluates the resulting program.

## Requirements

- GHC (the Glasgow Haskell Compiler)
- `runghc` (usually installed with GHC)
- Cabal (for building and running the test suite)

## Run

Pass an expression as command-line arguments:

```sh
runghc -isrc main.hs "10 + (3 * 4)"
```

Or provide an expression on standard input:

```sh
echo "10 + (3 * 4)" | runghc -isrc main.hs
```

Build the executable and run the tests with Cabal:

```sh
cabal build
cabal test
cabal run arithmetic-compiler -- "10 + (3 * 4)"
```

When invoking the compiled executable directly, quote expressions so the
shell does not expand operators such as `*`:

```sh
./arithmetic-compiler "2 * 3"
```

The program prints the compiled bytecode and the evaluated result:

```text
Bytecode: [Push 10,Push 3,Push 4,OpMul,OpAdd]
Result: 22
```

To compile an executable:

```sh
ghc -isrc -O2 main.hs -o arithmetic-compiler
./arithmetic-compiler "10 + (3 * 4)"
```

## Expression syntax

The compiler supports:

- Integer literals
- Parentheses
- Unary plus and minus (for example, `-5` or `-(2 + 3)`)
- Lexically scoped `let` bindings (for example, `let x = 5 in x * 2`)
- Addition (`+`) and subtraction (`-`)
- Multiplication (`*`) and integer division (`/`)
- Comparisons in conditions: `<`, `<=`, `==`, `!=`, `>=`, and `>`
- `if condition then expression else expression`; numeric conditions treat zero
  as false and every nonzero value as true

Multiplication and division have higher precedence than addition and
subtraction. Operators of the same precedence are evaluated left to right.
Division uses Haskell's integer `div` operation, which rounds toward negative
infinity (`-7 / 2` evaluates to `-4`).

Constant arithmetic is folded before code generation. Out-of-range integer
literals, undefined variables, malformed expressions, division by zero, and
the `minBound / -1` overflow case are reported as errors; errors cause the
program to exit unsuccessfully.

The test suite includes QuickCheck-generated expressions and verifies that
running compiled bytecode agrees with a separate tree-walking interpreter.
