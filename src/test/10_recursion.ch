# Test recursion for named functions
# Uses thunks (|>_. expr) to delay branch evaluation

~factorial n (
    (eq n 0 (|>_. 1) (|>_. n * factorial (n - 1))) 0
)

~fibonacci n (
    (eq n 0 (|>_. 0) (|>_. (eq n 1 (|>_. 1) (|>_. fibonacci (n - 1) + fibonacci (n - 2))) 0)) 0
)

~sum n (
    (eq n 0 (|>_. 0) (|>_. n + sum (n - 1))) 0
)

~(
    assert (eq (factorial 0) 1)
    assert (eq (factorial 1) 1)
    assert (eq (factorial 5) 120)
    assert (eq (factorial 10) 3628800)

    assert (eq (fibonacci 0) 0)
    assert (eq (fibonacci 1) 1)
    assert (eq (fibonacci 7) 13)

    assert (eq (sum 10) 55)
)
