# Test math standard library functions
import "math"

~(
    # sqrt
    assert (eq (sqrt 4.0) 2.0)
    assert (eq (sqrt 9.0) 3.0)
    assert (eq (sqrt 0.0) 0.0)

    # floor, ceil, round
    assert (eq (floor 3.7) 3)
    assert (eq (ceil 3.2) 4)
    assert (eq (round 3.5) 4)
    assert (eq (round 3.4) 3)

    # abs
    assert (eq (abs 5) 5)
    assert (eq (abs 0) 0)

    # pow
    assert (eq (pow 2.0 3.0) 8.0)
    assert (eq (pow 3.0 2.0) 9.0)
    assert (eq (pow 2.0 0.0) 1.0)

    # min, max
    assert (eq (min 3.0 5.0) 3.0)
    assert (eq (max 3.0 5.0) 5.0)

    # trig: sin(0) = 0, cos(0) = 1
    assert (eq (sin 0.0) 0.0)
    assert (eq (cos 0.0) 1.0)
)
