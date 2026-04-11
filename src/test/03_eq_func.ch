# Test equality and conditional function selection

@v1 false
@v2 true

~(
    assert (eq v1 false)
    assert (eq v2 true)

    # eq as conditional: eq a b trueBranch falseBranch
    assert (eq (eq 1 1 "foo" "bar") "foo")
    assert (eq (eq 1 2 "foo" "bar") "bar")
)
