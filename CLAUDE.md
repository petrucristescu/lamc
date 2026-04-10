# LMC — Lambda Mini Calculus

An interpreter for a functional programming language, written in OCaml.

## Project Structure

- `src/lmc.ml` — Entry point: parse → type-infer → evaluate
- `src/parser.ml` — Lexer, parser, and `parse_and_infer` (type errors are non-fatal warnings)
- `src/ast.ml` — AST: Int, Lng, Float, Str, Add, Sub, Mul, Div, Eq, Var, Lam, App, Let, FunDef, Seq, Print, Import
- `src/eval.ml` — Evaluator: environment-based closures, VRecFun for recursion, VTailCall trampoline for TCO
- `src/infer.ml` — Hindley-Milner type inference with unification
- `src/types.ml` — Type definitions (TInt, TLong, TFloat, TString, TBool, TVar, TFun, TUnknown)
- `src/lib/` — Standard libraries (operators.lmc)
- `src/test/` — Tests: `*.lmc` files with expected output in `*.lmc.out`

## Language Syntax

```
~name arg1,arg2  body        # Named function (comma-separated args)
|>param. body                # Lambda expression
@type_var value              # Variable declaration (@i_x 5 for int)
~( expr1 expr2 ... )         # Main block
eq a b                       # Equality (returns Church boolean)
print expr                   # Output
import "lib"                 # Import library
```

## Build & Test

No OCaml installed locally. Use Docker:

```bash
docker build -t lamc-test .     # first time only (installs OCaml + dune)
./run-tests.sh                  # run all tests
./run-tests.sh 12_lambda        # run a single test by name
```

This replicates the GitHub Actions CI (ubuntu:22.04, opam 2.1.5, OCaml 5.1.1, dune 3.8).

The script mounts the local source as a volume, so file changes are picked up without rebuilding the image.

## Testing Conventions

- Each test: `src/test/NN_name.lmc` with expected output in `src/test/NN_name.lmc.out`
- Expected output files must have a trailing newline
- CI runs all tests via diff: `_build/default/src/lmc.exe test.lmc | diff - test.lmc.out`

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