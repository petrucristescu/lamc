# Test nested eq conditionals
@v1 10
@v2 12

assert (eq (eq v1 v2 "v1 equals v2" "v1 not equals v2") "v1 not equals v2")
assert (eq (eq v1 v2 "v1 equals v2" (eq v1 10
    "v1 equals 10"
    "v1 not equals 10"
)) "v1 equals 10")
true
