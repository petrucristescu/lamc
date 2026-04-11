# List Library
# Native primitives: nil, cons, head, tail, empty, len, nth, reverse,
#   range, map, filter, foldl, foldr, matchList, matchBool

# Sum all elements
~sum l (foldl (|>acc. |>x. acc + x) 0 l)

# Product of all elements
~product l (foldl (|>acc. |>x. acc * x) 1 l)

# Check if any element satisfies predicate
~any f,l (foldl (|>acc. |>x. or acc (f x)) false l)

# Check if all elements satisfy predicate
~all f,l (foldl (|>acc. |>x. and acc (f x)) true l)

# Take first n elements
~take n,l (
    matchList l
        (|>_. [])
        (|>h. |>t. eq n 0
            (|>_. [])
            (|>_. cons h (take (n - 1) t))
        0))

# Drop first n elements
~drop n,l (
    matchList l
        (|>_. [])
        (|>h. |>t. eq n 0
            (|>_. cons h t)
            (|>_. drop (n - 1) t)
        0))

# Zip two lists into a list of pairs (as 2-element lists)
~zip a,b (
    matchList a
        (|>_. [])
        (|>ha. |>ta. matchList b
            (|>_. [])
            (|>hb. |>tb. cons [ha, hb] (zip ta tb))))

# Flatten a list of lists
~flatten l (foldr (|>x. |>acc. foldl (|>a. |>e. cons e a) acc (reverse x)) [] l)

# Append two lists
~append a,b (foldr (|>x. |>acc. cons x acc) b a)
