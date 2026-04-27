import "vector"

# vecAdd
assert (eq (vecAdd [1.0, 2.0, 3.0] [4.0, 5.0, 6.0]) [5.0, 7.0, 9.0])

# vecSub
assert (eq (vecSub [5.0, 7.0, 9.0] [4.0, 5.0, 6.0]) [1.0, 2.0, 3.0])

# vecMul
assert (eq (vecMul [2.0, 3.0] [4.0, 5.0]) [8.0, 15.0])

# vecScale
assert (eq (vecScale 2.0 [1.0, 2.0, 3.0]) [2.0, 4.0, 6.0])

# vecDot
assert (eq (vecDot [1.0, 2.0, 3.0] [4.0, 5.0, 6.0]) 32.0)

# vecSum
assert (eq (vecSum [1.0, 2.0, 3.0, 4.0]) 10.0)

# vecZeros
assert (eq (vecZeros 3) [0.0, 0.0, 0.0])
assert (eq (len (vecZeros 5)) 5)

# vecRandom
@rv (vecRandom 4 1.0)
assert (eq (len rv) 4)

# argmax
assert (eq (argmax [0.1, 0.9, 0.3]) 1)
assert (eq (argmax [0.1, 0.2, 0.8, 0.3]) 2)
assert (eq (argmax [5.0, 1.0, 2.0]) 0)

# vecZipWith
@added (vecZipWith (|>a. |>b. a + b) [1.0, 2.0] [3.0, 4.0])
assert (eq added [4.0, 6.0])

true
