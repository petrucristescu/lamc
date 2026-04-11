# Test zero-arg function definitions and evaluation
~foo "foo called"
~bar "bar called"

~(
    assert (eq foo "foo called")
    assert (eq bar "bar called")
)
