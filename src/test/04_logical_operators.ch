# Test boolean operators

# not
assert (eq (not false) true)
assert (eq (not true) false)

# and
assert (and true true)
assert (not (and true false))
assert (not (and false true))
assert (not (and false false))

# or
assert (not (or false false))
assert (or true false)
assert (or false true)

# chained
assert (not (and true false))
assert (or false true)

# complex
assert (and (or true false) (not false))
true
