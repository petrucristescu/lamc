# Churing

A functional programming language named after [Alonzo Church](https://en.wikipedia.org/wiki/Alonzo_Church) and [Alan Turing](https://en.wikipedia.org/wiki/Alan_Turing) — the founders of computation theory. Church invented lambda calculus; Turing defined the universal machine. Churing brings both ideas together in one language.

## Core Features

### Type System
- Basic types: `Int`, `Long` (64-bit integers), `Float`, `Bool`, `String`
- Function types
- Hindley-Milner type inference with let-polymorphism

### Expressions
- Numeric literals: integers (`123`), longs (`123L`), floats (`3.14`, `.5`, `123f`)
- String literals (in double quotes: `"hello"`)
- Boolean literals (`true`, `false`)
- Variables
- Lambda functions (`|>x. body`)
- Function application
- Arithmetic operations (`+`, `-`, `*`, `/`) with proper precedence
- Equality testing (`eq`)
- Assertions (`assert`)

### Declarations
- Variable declarations with type inference (`@i_x 5` for int, `@s_name "hello"` for string)
- Named function definitions (`~add x,y x + y`)
- Recursive functions with tail call optimization

### I/O
- `print` expression for output
- `import` for loading libraries

## Syntax Examples

```
# Variable declarations
@i_v1 4        # Integer
@l_v3 4L       # Long integer
@f_v5 2.4      # Float
@s_v2 "string" # String

# Function definitions
~add x,y x + y
~factorial n (
    (eq n 0 (|>_. 1) (|>_. n * factorial (n - 1))) 0
)

# Lambda expressions
~compose f,g,x (f (g x))

# Main program block
~(
    print (add 2 3)
    assert (eq (factorial 5) 120)
    print (compose (|>x. x + 1) (|>x. x * 10) 3)
)
```

## Implementation

Churing is implemented in OCaml with several key components:

1. **Lexer/Parser** (`parser.ml`): Tokenizes and parses source code into an AST
2. **AST** (`ast.ml`): Defines the abstract syntax tree
3. **Type System** (`types.ml`): Defines types, schemes, and type operations
4. **Type Inference** (`infer.ml`): Implements Hindley-Milner type inference with unification
5. **Evaluator** (`eval.ml`): Interprets the AST with closures, recursion, and tail call optimization

## Features

- **Type inference**: Types are inferred automatically via Hindley-Milner
- **First-class functions**: Functions can be passed as arguments and returned as values
- **Tail call optimization**: Deep recursion without stack overflow
- **Native booleans**: `true`/`false` with Church-style applicability (`true a b = a`)
- **Assertions**: Built-in `assert` for self-checking tests
- **Smart number formatting**: Whole number results displayed without decimal points

## Usage

```bash
# Build (requires Docker)
docker build -t churing-test .

# Run a program
./run.sh example.ch

# Run all tests (unit + integration)
./run-tests.sh

# Run a single test
./run-tests.sh 12_lambda
```

## Design Philosophy

Churing honours its namesakes by combining Church's lambda calculus with Turing's practical computability. It serves as both an educational tool for understanding functional programming and a testbed for language implementation techniques — parsing, type inference, closures, and tail call optimization.
