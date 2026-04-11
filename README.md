# Churing

A functional programming language named after [Alonzo Church](https://en.wikipedia.org/wiki/Alonzo_Church) and [Alan Turing](https://en.wikipedia.org/wiki/Alan_Turing) — the founders of computation theory. Church invented lambda calculus; Turing defined the universal machine. Churing brings both ideas together in one language.

## Quick Example

```
~fib n (match n | 0 -> 0 | 1 -> 1 | x -> fib (x - 1) + fib (x - 2))

fib 10
```
Output: `55`

No boilerplate — define functions and reduce to a value. The last expression is the program's return value.

## Core Features

### Pure Lambda Calculus Feel
- No `print` — programs reduce to a single return value
- No `main` block required — top-level expressions execute directly
- Standard library auto-loaded — no import statements needed
- Church-style booleans: `true a b = a`, `false a b = b`

### Type System
- Basic types: `Int`, `Long` (64-bit), `Float`, `Bool`, `String`, `List`
- Function types (curried)
- Hindley-Milner type inference with let-polymorphism

### Pattern Matching
```
~eval expr (match expr
    | 0 -> "zero"
    | n -> concat "number: " (toString n))

~len l (match l
    | [] -> 0
    | h :: t -> 1 + len t)
```
Patterns: literals, variables, wildcards (`_`), lists (`[x, y]`), cons destructuring (`h :: t`).

### Lists (Three Approaches)
```
# Native lists with literal syntax
@nums [1, 2, 3, 4, 5]
map (|>x. x * 2) nums              # [2, 4, 6, 8, 10]
foldl (|>acc. |>x. acc + x) 0 nums # 15

# Cons cells (Lisp-style)
cons 0 nums                         # [0, 1, 2, 3, 4, 5]

# Church-encoded lists (pure lambda calculus)
@cl (church_cons 1 (church_cons 2 church_nil))
church_sum cl                       # 3
```

### Error Handling (Two Approaches)
```
# try/catch primitive
@safe (try (readFile "missing.txt") (|>err. "default"))

# Church-encoded Result type
@result (ok 42)
unwrapOr 0 result                   # 42
```

### Expressions
- Numeric literals: integers (`123`), longs (`123L`), floats (`3.14`, `.5`, `123f`)
- String literals (`"hello"`)
- Boolean literals (`true`, `false`)
- Lambda functions (`|>x. body`)
- Curried function application
- Arithmetic (`+`, `-`, `*`, `/`) with proper precedence
- Equality (`eq`)
- Assertions (`assert`)

### Declarations
```
@x 5                        # Variable (type inferred)
~add x,y x + y              # Named function (comma-separated args)
~fact n (match n             # Recursive (with tail call optimization)
    | 0 -> 1
    | n -> n * fact (n - 1))
```

## Standard Library

All functions are available without imports:

| Library | Functions |
|---------|-----------|
| **operators** | `true`, `false`, `not`, `and`, `or`, `if`, `identity`, `const`, `flip`, `compose` |
| **math** | `sqrt`, `sin`, `cos`, `tan`, `asin`, `acos`, `atan`, `floor`, `ceil`, `round`, `abs`, `pow`, `min`, `max`, `pi`, `e`, `square`, `cube`, `sign`, `clamp`, `lerp` |
| **string** | `length`, `concat`, `substring`, `uppercase`, `lowercase`, `trim`, `charAt`, `indexOf`, `startsWith`, `endsWith`, `replace`, `toString`, `isEmpty`, `contains`, `repeat`, `padLeft` |
| **list** | `nil`, `cons`, `head`, `tail`, `empty`, `len`, `nth`, `reverse`, `range`, `map`, `filter`, `foldl`, `foldr`, `matchList`, `matchBool`, `sum`, `product`, `any`, `all`, `take`, `drop`, `zip`, `flatten`, `append` |
| **time** | `now`, `timeMs`, `year`, `month`, `day`, `hour`, `minute`, `second`, `dayOfWeek`, `diffTime`, `isLeapYear` |
| **io** | `readFile`, `writeFile`, `appendFile`, `fileExists`, `deleteFile`, `readLines`, `writeLines`, `pureIO`, `bindIO`, `mapIO`, `runIO` |
| **result** | `ok`, `err`, `matchResult`, `mapResult`, `bindResult`, `unwrapOr`, `isOk`, `isErr` |
| **church_list** | `church_nil`, `church_cons`, `church_head`, `church_sum`, `church_map`, `church_fold`, `church_length` |

## Implementation

Churing is implemented in OCaml:

1. **Lexer/Parser** (`src/parser.ml`) — Tokenizes and parses source into an AST
2. **AST** (`src/ast.ml`) — Abstract syntax tree with pattern matching support
3. **Type System** (`src/types.ml`) — Types, schemes, and type operations including `TList`
4. **Type Inference** (`src/infer.ml`) — Hindley-Milner with unification, generalization, and polymorphic list operations
5. **Evaluator** (`src/eval.ml`) — Environment-based interpreter with closures, recursion, tail call optimization, native lists, cons cells, and pattern matching
6. **Standard Library** (`src/lib/*.ch`) — Hybrid: native OCaml primitives + Churing-level helpers

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

Churing honours its namesakes by combining Church's lambda calculus with Turing's practical computability. The language aims to feel like pure lambda calculus — programs are expressions that reduce to values — while providing practical features (pattern matching, lists, I/O, error handling) through both native primitives and Church-encoded alternatives.

Every major feature offers two approaches:
- **Native** — practical and efficient
- **Church-encoded** — pure lambda calculus, implemented in Churing itself

This lets users choose their level of abstraction: use native lists for convenience, or Church-encoded lists to stay true to lambda calculus.
