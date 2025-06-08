# LMC: Lambda Mini Calculus

LMC is a programming language that combines elements of [lambda calculus](https://en.wikipedia.org/wiki/Lambda_calculus) with practical programming features, implemented in OCaml.

## Core Features

### Type System
- Basic types: `Int`, `Long` (64-bit integers), `Float`, `Bool`, `String`
- Function types
- Hindley-Milner type inference

### Expressions
- Numeric literals: integers (`123`), longs (`123l`), floats (`3.14`, `.5`, `123f`)
- String literals (in double quotes: `"hello"`)
- Boolean literals (`true`, `false`)
- Variables
- Lambda functions
- Function application
- Arithmetic operations (`+`, `-`, `*`, `/`)
- Equality testing (`eq`)

### Declarations
- Variable declarations (with optional type annotations)
- Function definitions

### I/O
- `print` expression for output

## Syntax Examples

```lmc
// Variable declarations
@i_v1 4        // Integer
@l_v3 4l       // Long integer
@f_v5 2.4      // Float
@s_v2 "string" // String

// Function definitions
~add x,y x + y
~mul x,y x * y
~div x,y x / y

// Main program block
~(
    print (add 2 3)
    print (div 9 4)   // Returns 2.25 as a float
)
```

## Implementation

LMC is implemented in OCaml with several key components:

1. **Lexer/Parser** (`parser.ml`): Tokenizes and parses the source code into an abstract syntax tree
2. **AST** (`ast.ml`): Defines the abstract syntax tree for the language
3. **Type System** (`types.ml`): Defines types and type operations
4. **Type Inference** (`infer.ml`): Implements Hindley-Milner type inference
5. **Evaluator** (`eval.ml`): Interprets the AST to execute programs

## Features

- **Type inference**: Types are inferred automatically
- **First-class functions**: Functions can be passed as arguments and returned as values
- **Floating-point division**: Division always gives precise floating-point results
- **Smart number formatting**: Whole number results are displayed without decimal points

## Usage

```bash
# Run a program
./lmc.exe examples/operators.lmc

# Run with type inference output
./lmc.exe --show-types examples/operators.lmc
```

## Design Philosophy

LMC combines functional programming concepts with practical features like arithmetic operations and I/O. It serves as an educational tool for understanding lambda calculus and functional programming concepts while providing practical utility.
