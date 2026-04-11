# Test the assert primitive itself

~(
    assert true
    assert (not false)

    assert (eq 1 1)
    assert (eq "hello" "hello")
    assert (eq true true)

    assert (not (eq 1 2))
    assert (not (eq true false))

    assert (and true true)
    assert (or true false)
    assert (or false true)
    assert (not (and true false))
    assert (not (or false false))
)
