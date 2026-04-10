# Test lambda expressions (|>param. body)

~identity x ((|>a. a) x)
~inc x ((|>a. a + 1) x)
~double x ((|>a. a * 2) x)
~curried_add x,y ((|>a. |>b. a + b) x y)
~apply f,x (f x)
~compose f,g,x (f (g x))

~(
    assert (eq (identity 42) 42)
    assert (eq (inc 9) 10)
    assert (eq (double 5) 10)

    assert (eq (curried_add 3 4) 7)

    assert (eq (apply (|>n. n + 100) 5) 105)
    assert (eq (apply (|>n. n * 3) 7) 21)

    assert (eq (compose (|>x. x + 1) (|>x. x * 10) 3) 31)
    assert (eq (compose (|>x. x * 2) (|>x. x + 5) 10) 30)
    assert (eq (compose (|>x. x * 4) (|>x. x + 10) 10) 80)

    assert (eq ((|>x. x * 5) 5) 25)
)
