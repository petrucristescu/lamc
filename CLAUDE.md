# Churing

A functional programming language named after Alonzo Church and Alan Turing, implemented in OCaml.

## Project Structure

- `src/churing.ml` — Entry point: parse → type-infer → evaluate
- `src/parser.ml` — Lexer, parser, and `parse_and_infer` (type errors are non-fatal warnings)
- `src/ast.ml` — AST: Int, Lng, Float, Str, Bool, Add, Sub, Mul, Div, Eq, Var, Lam, App, Let, FunDef, Seq, Print, Assert, Import
- `src/eval.ml` — Evaluator: environment-based closures, VRecFun for recursion, VTailCall trampoline for TCO
- `src/infer.ml` — Hindley-Milner type inference with unification and type schemes
- `src/types.ml` — Type definitions (TInt, TLong, TFloat, TString, TBool, TVar, TFun, TUnknown)
- `src/lib/` — Standard libraries (operators.ch)
- `src/test/` — Integration tests: `*.ch` files, assert-based or with `*.ch.out` for output diff
- `test/` — OCaml unit tests (Alcotest): parser, evaluator, type inference

## Language Syntax

```
~name arg1,arg2  body        # Named function (comma-separated args)
|>param. body                # Lambda expression
@type_var value              # Variable declaration (@i_x 5 for int)
~( expr1 expr2 ... )         # Main block
eq a b                       # Equality (returns boolean)
assert expr                  # Assertion (fails with exit 1 if false)
print expr                   # Output
import "lib"                 # Import library
```

## Build & Test

No OCaml installed locally. Use Docker:

```bash
docker build -t churing-test .     # first time only (installs OCaml + dune)
./run-tests.sh                     # run all tests (unit + integration)
./run-tests.sh 12_lambda           # run a single test by name
./run.sh example.ch                # run a Churing file
```

This closely replicates the GitHub Actions CI (ubuntu:22.04, opam 2.1.5, OCaml 5.1.1, dune >= 3.8).

The script mounts the local source as a volume, so file changes are picked up without rebuilding the image.

## Testing Conventions

- Assert-based tests: `src/test/NN_name.ch` with `assert` statements, no `.out` file needed
- Output-based tests: `src/test/NN_name.ch` with expected output in `src/test/NN_name.ch.out`
- OCaml unit tests: `test/test_parser.ml`, `test/test_eval.ml`, `test/test_infer.ml`
- CI runs `dune runtest` (unit tests) then integration tests

## GitHub CLI

The `GITHUB_TOKEN` env var is invalid in this environment. Prefix gh commands with `GITHUB_TOKEN=`:

```bash
GITHUB_TOKEN= gh issue list
GITHUB_TOKEN= gh pr create ...
```

## Commit Conventions

- Follow existing style: `feat:`, `fix:` prefixes with issue reference `(#N)`
- Author: petru.cristescu@gmail.com (set in local git config)
- No Co-Authored-By or Claude references in commits
