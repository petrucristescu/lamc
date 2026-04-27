# Test native float array primitives

# arrayCreate, arrayGet, arrayLength
@a (arrayCreate 5 0.0)
assert (eq (arrayLength a) 5)
assert (eq (arrayGet a 0) 0.0)
assert (eq (arrayGet a 4) 0.0)

# arraySet returns new array
@b (arraySet a 2 3.14)
assert (eq (arrayGet b 2) 3.14)
assert (eq (arrayGet a 2) 0.0)

# arrayFromList / arrayToList roundtrip
@c (arrayFromList [1.0, 2.0, 3.0])
assert (eq (arrayLength c) 3)
assert (eq (arrayGet c 1) 2.0)
assert (eq (arrayToList c) [1.0, 2.0, 3.0])

# arrayDot
@d1 (arrayFromList [1.0, 2.0, 3.0])
@d2 (arrayFromList [4.0, 5.0, 6.0])
assert (eq (arrayDot d1 d2) 32.0)

# arrayVecAdd / arrayVecSub
@sum (arrayVecAdd d1 d2)
assert (eq (arrayGet sum 0) 5.0)
assert (eq (arrayGet sum 2) 9.0)
@diff (arrayVecSub d2 d1)
assert (eq (arrayGet diff 0) 3.0)

# arrayVecMul (element-wise)
@prod (arrayVecMul d1 d2)
assert (eq (arrayGet prod 0) 4.0)
assert (eq (arrayGet prod 1) 10.0)

# arrayVecScale
@scaled (arrayVecScale 2.0 d1)
assert (eq (arrayGet scaled 0) 2.0)
assert (eq (arrayGet scaled 2) 6.0)

# arraySum
assert (eq (arraySum d1) 6.0)

# arrayArgmax
@e (arrayFromList [0.1, 0.9, 0.3])
assert (eq (arrayArgmax e) 1)

# arrayRandom
@r (arrayRandom 10 1.0)
assert (eq (arrayLength r) 10)

# arraySoftmax
@sm (arraySoftmax (arrayFromList [1.0, 2.0, 3.0]))
assert (lt (abs ((arraySum sm) - 1.0)) 0.0001)
assert (gt (arrayGet sm 2) (arrayGet sm 0))

# arrayRelu
@relu_result (arrayRelu (arrayFromList [1.0, 0.0 - 2.0, 3.0, 0.0 - 0.5]))
assert (eq (arrayGet relu_result 0) 1.0)
assert (eq (arrayGet relu_result 1) 0.0)
assert (eq (arrayGet relu_result 2) 3.0)

# arrayReluDeriv
@rd (arrayReluDeriv (arrayFromList [1.0, 0.0 - 2.0, 0.0]))
assert (eq (arrayGet rd 0) 1.0)
assert (eq (arrayGet rd 1) 0.0)
assert (eq (arrayGet rd 2) 0.0)

# arrayMatVecMul
@mat [arrayFromList [1.0, 0.0], arrayFromList [0.0, 1.0]]
@vec (arrayFromList [3.0, 4.0])
@result (arrayMatVecMul mat vec)
assert (eq (arrayGet result 0) 3.0)
assert (eq (arrayGet result 1) 4.0)

# arrayOuterProduct
@op (arrayOuterProduct (arrayFromList [1.0, 2.0]) (arrayFromList [3.0, 4.0]))
assert (eq (len op) 2)

# arrayMatTranspose
@mt (arrayMatTranspose [arrayFromList [1.0, 2.0, 3.0], arrayFromList [4.0, 5.0, 6.0]])
assert (eq (len mt) 3)

# arrayMatAdd
@ma [arrayFromList [1.0, 2.0], arrayFromList [3.0, 4.0]]
@mb [arrayFromList [5.0, 6.0], arrayFromList [7.0, 8.0]]
@mc (arrayMatAdd ma mb)
assert (eq (len mc) 2)

# arrayMatScale
@ms (arrayMatScale 2.0 [arrayFromList [1.0, 2.0]])
assert (eq (len ms) 1)
