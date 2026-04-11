# Math Library
# Native primitives: sqrt, sin, cos, tan, asin, acos, atan,
#   floor, ceil, round, abs, pow, min, max

# Constants (approximations)
@pi 3.14159265358979323846
@e 2.71828182845904523536

# Additional helpers
~square x (x * x)
~cube x (x * x * x)
~sign x (
    eq x 0
        (|>_. 0)
        (|>_. eq (abs x) x
            (|>_. 1)
            (|>_. 0 - 1)
        0)
    0
)
~clamp lo,hi,x (min hi (max lo x))
~lerp a,b,t (a + t * (b - a))
