# Test Churing-level helpers defined in library .ch files
import "operators"
import "math"
import "string"
import "list"

~(
    # operators.ch: identity, const, compose
    assert (eq (identity 42) 42)
    assert (eq (const 1 2) 1)
    assert (eq (compose (|>x. x + 1) (|>x. x * 2) 5) 11)
    assert (eq (flip sub 3 10) 7)

    # math.ch: pi, square, cube, clamp, lerp
    assert (eq (square 5) 25)
    assert (eq (cube 3) 27)
    assert (eq (clamp 0.0 10.0 15.0) 10.0)
    assert (eq (clamp 0.0 10.0 5.0) 5.0)
    assert (eq (lerp 0.0 10.0 0.5) 5.0)

    # string.ch: isEmpty, contains, repeat
    assert (isEmpty "")
    assert (not (isEmpty "hi"))
    assert (contains "hello world" "world")
    assert (not (contains "hello" "xyz"))
    assert (eq (repeat "ab" 3) "ababab")
    assert (eq (repeat "x" 0) "")

    # list.ch: sum, product, any, all, take, drop, append
    assert (eq (sum [1, 2, 3, 4, 5]) 15)
    assert (eq (product [1, 2, 3, 4]) 24)
    assert (any (|>x. eq x 3) [1, 2, 3])
    assert (not (any (|>x. eq x 9) [1, 2, 3]))
    assert (all (|>x. not (eq x 0)) [1, 2, 3])
    assert (eq (take 2 [1, 2, 3]) [1, 2])
    assert (eq (drop 2 [1, 2, 3]) [3])
    assert (eq (append [1, 2] [3, 4]) [1, 2, 3, 4])
)
