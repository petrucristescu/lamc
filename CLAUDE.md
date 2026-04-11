# Churing

A functional programming language named after Alonzo Church and Alan Turing, implemented in OCaml.

## Project Structure

- `src/churing.ml` — Entry point: parse → type-infer → evaluate
- `src/parser.ml` — Lexer, parser, and `parse_and_infer` (type errors are non-fatal warnings)
- `src/ast.ml` — AST: Int, Lng, Float, Str, Bool, Add, Sub, Mul, Div, Eq, Var, Lam, App, Let, FunDef, Seq, Assert, List, Match, Try, Import
- `src/eval.ml` — Evaluator: environment-based closures, VRecFun for recursion, VTailCall trampoline for TCO, VList/VCons/VNil for lists
- `src/infer.ml` — Hindley-Milner type inference with unification and type schemes
- `src/types.ml` — Type definitions (TInt, TLong, TFloat, TString, TBool, TVar, TFun, TList, TUnknown)
- `src/lib/` — Standard library `.ch` files (auto-loaded at startup)
- `src/test/` — Integration tests: `*.ch` files, assert-based
- `test/` — OCaml unit tests (Alcotest): parser, evaluator, type inference

## Language Syntax

Programs are pure: the last expression is the program's return value (auto-printed). No `print`, no `~()` main block required.

```
~name arg1,arg2  body        # Named function (comma-separated args)
|>param. body                # Lambda expression
@name value                  # Variable declaration (@x 5)
eq a b                       # Equality (returns boolean)
assert expr                  # Assertion (fails with exit 1 if false)
match expr | pat -> body     # Pattern matching (literals, variables, wildcards, lists, cons)
try expr (|>err. handler)    # Error handling (catches runtime errors)
[1, 2, 3]                   # List literal
h :: t                       # Cons pattern (head :: tail destructuring)
import "lib"                 # Import custom library (stdlib is auto-loaded)
```

## Standard Library (auto-loaded)

All standard library functions are available without imports. Each library has native primitives (OCaml) plus Churing-level helpers in `src/lib/*.ch`.

- **operators**: true, false, not, and, or, if, identity, const, flip, compose
- **math**: sqrt, sin, cos, tan, asin, acos, atan, floor, ceil, round, abs, pow, min, max, pi, e, square, cube, sign, clamp, lerp
- **string**: length, concat, substring, uppercase, lowercase, trim, charAt, indexOf, startsWith, endsWith, replace, toString, isEmpty, contains, repeat, padLeft
- **list**: nil, cons, head, tail, empty, len, nth, reverse, range, map, filter, foldl, foldr, matchList, matchBool, sum, product, any, all, take, drop, zip, flatten, append
- **time**: now, timeMs, year, month, day, hour, minute, second, dayOfWeek, diffTime, isLeapYear
- **io**: readFile, writeFile, appendFile, fileExists, deleteFile, readLines, writeLines, pureIO, bindIO, mapIO, seqIO, runIO, readFileIO, writeFileIO, appendFileIO, deleteFileIO, readLinesIO, writeLinesIO
- **church_list**: church_nil, church_cons, church_head, church_sum, church_map, church_fold, church_length
- **result**: ok, err, matchResult, mapResult, bindResult, unwrapOr, isOk, isErr

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

- Assert-based tests: `src/test/NN_name.ch` with `assert` statements (primary approach)
- Output-based tests: `src/test/NN_name.ch` with expected output in `src/test/NN_name.ch.out` (for testing last-value output)
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
