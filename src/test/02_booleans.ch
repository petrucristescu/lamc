# Test boolean values and equality
@v1 true
@v2 false

assert (eq v1 true)
assert (eq v2 false)
assert (eq v1 v1)
assert (not (eq v1 v2))
true
