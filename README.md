# LAMC
This is a compiler built using OCAML

The intention is here is to build something as close as possible to the Lambda Calculus.

The name comes from the lambda+calculus

To learn more about the lambda calculus, you can visit [Wikipedia](https://en.wikipedia.org/wiki/Lambda_calculus).

### Prerequisites

OCaml is required to build this compiler. You can install it using OPAM, the OCaml package manager.

### Building the Compiler

dune build

### Testing the Compiler

dune exec src/lmc.exe test.lmc

### Feature Flags

You can control the compiler's output using the following command-line flags:

- `--ast`  
  Prints the parsed Abstract Syntax Tree (AST) for the input file.

- `--result`  
  Evaluates every expression in the file and prints the result for each line, including variable and function definitions.

#### Example Usage

```sh
dune exec src/lmc.exe -- --ast test.lmc
dune exec src/lmc.exe -- --result test.lmc

