# LMC: Lambda Mini Calculus

Welcome to the LMC (Lambda Mini Calculus) wiki! This page provides comprehensive documentation of the language syntax, features, and usage.

LMC is a functional programming language inspired by lambda calculus that combines theoretical concepts with practical programming features. It features a strong type system with type inference and first-class functions.

## Table of Contents

- [Syntax Overview](#syntax-overview)
- [Type System](#type-system)
- [Variables and Constants](#variables-and-constants)
- [Functions](#functions)
- [Operators](#operators)
- [Control Flow](#control-flow)
- [Input and Output](#input-and-output)
- [Examples](#examples)

## Syntax Overview

LMC programs consist of variable declarations, function definitions, and a main expression block:

```lmc
// Variable declarations start with @
@i_x 42        // Integer
@l_y 123l      // Long (64-bit integer)
@f_z 3.14      // Float
@s_greeting "Hello, World!"  // String

// Function definitions start with ~
~add x,y x + y  // A function that adds two values

// Main program block
~(
    print (add 2 3)
)
```

## Type System

LMC has a Hindley-Milner type system with type inference. The basic types are:

| Type | Description | Example |
|------|-------------|---------|
| `Int` | 32-bit integer | `42` |
| `Long` | 64-bit integer | `42l` |
| `Float` | Floating-point number | `3.14`, `.5`, `1.0f` |
| `String` | Text string | `"Hello"` |
| `Bool` | Boolean value | `true`, `false` |
| Function types | `a -> b` | Functions from type `a` to type `b` |

### Type Annotations

Variables can be given explicit types with naming conventions:

```lmc
@i_num 42    // i_ prefix for Int
@l_big 42l   // l_ prefix for Long
@f_pi 3.14   // f_ prefix for Float
@s_msg "hi"  // s_ prefix for String
```

## Variables and Constants

Variables are declared using the `@` symbol followed by a name and a value:

```lmc
@i_x 42
@f_pi 3.14159
@s_name "Alice"
```

## Functions

### Function Definition

Functions are defined using the `~` symbol, followed by the function name, parameter list, and body:

```lmc
~add x,y x + y
~square x x * x
```

### Function Application

Functions are called by placing arguments after the function name:

```lmc
add 2 3       // Returns 5
square 4      // Returns 16
```

Functions can be composed:

```lmc
~compose f,g,x f (g x)
~square x x * x
~double x x + x
compose double square 3   // double(square(3)) = double(9) = 18
```

## Operators

LMC supports the following operators:

| Operator | Description | Example |
|----------|-------------|---------|
| `+` | Addition | `x + y` |
| `-` | Subtraction | `x - y` |
| `*` | Multiplication | `x * y` |
| `/` | Division (returns Float) | `x / y` |
| `eq` | Equality | `eq x y` |

The equality operator `eq` can be used for conditional execution:

```lmc
eq x 0 "It's zero" "It's not zero"
```

## Control Flow

### Conditional Execution

LMC uses the `eq` function for conditions:

```lmc
eq x 0 "Zero" "Not Zero"
```

This evaluates `x == 0`, and if true, returns the first expression (`"Zero"`), otherwise returns the second expression (`"Not Zero"`).

## Input and Output

### Output

The `print` function outputs values to the console:

```lmc
print "Hello, World!"
print (add 2 3)
```

## Examples

### Basic Arithmetic

```lmc
~(
    print (5 + 3)        // 8
    print (10 - 4)       // 6
    print (3 * 4)        // 12
    print (9 / 2)        // 4.5 (floating point)
)
```

### Functions and Composition

```lmc
~square x x * x
~inc x x + 1
~(
    print (square 4)          // 16
    print (inc 5)             // 6
    print (square (inc 3))    // 16 (square of 4)
)
```

### Conditionals

```lmc
~is_positive x eq x 0 "Zero" (eq (x < 0) true "Negative" "Positive")
~(
    print (is_positive 5)     // "Positive"
    print (is_positive 0)     // "Zero"
    print (is_positive -3)    // "Negative"
)
```

### Working with Different Types

```lmc
@i_age 30
@f_pi 3.14159
@s_greeting "Hello"
@l_bigNumber 9223372036854775807l

~(
    print (i_age + 1)        // 31
    print (f_pi * 2)         // 6.28318
    print (s_greeting)       // "Hello"
    print (l_bigNumber)      // 9223372036854775807
)
```
