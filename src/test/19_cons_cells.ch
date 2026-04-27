# Test cons cell operations (Approach C — Lisp-style)

# Build list from cons/nil
@mylist (cons 1 (cons 2 (cons 3 [])))

# cons builds a proper list
assert (eq mylist [1, 2, 3])

# head and tail on cons-built lists
assert (eq (head mylist) 1)
assert (eq (tail mylist) [2, 3])

# empty on empty
assert (empty [])
assert (not (empty mylist))

# Nested cons with nil
assert (eq (cons 10 []) [10])
assert (eq (cons 1 (cons 2 [])) [1, 2])

# map/filter/fold work on cons-built lists
assert (eq (map (|>x. x * 10) mylist) [10, 20, 30])
assert (eq (foldl (|>acc. |>x. acc + x) 0 mylist) 6)
