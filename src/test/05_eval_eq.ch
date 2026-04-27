# Test eq as conditional expression
@v1 9
@v2 9

assert (eq (eq v1 v2 "foo" "bar") "foo")
assert (eq (eq v1 10 "foo" "bar") "bar")
true
