# Operators Library
# Native primitives: true, false, not, and, or, if
# This file adds Churing-level helpers on top

# Basic arithmetic wrappers
~add x,y x + y
~sub x,y x - y
~mul x,y x * y

# Utility functions
~identity x x
~const x,y x
~flip f,x,y (f y x)
~compose f,g,x (f (g x))
