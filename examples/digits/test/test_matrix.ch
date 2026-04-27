import "matrix"

# matVecMul: identity-like matrix
@mat [[1.0, 0.0], [0.0, 1.0]]
@vec [3.0, 4.0]
assert (eq (matVecMul mat vec) [3.0, 4.0])

# matVecMul: scaling matrix
@mat2 [[2.0, 0.0], [0.0, 3.0]]
assert (eq (matVecMul mat2 vec) [6.0, 12.0])

# matTranspose
@m [[1.0, 2.0, 3.0], [4.0, 5.0, 6.0]]
@mt (matTranspose m)
assert (eq (len mt) 3)
assert (eq (nth mt 0) [1.0, 4.0])
assert (eq (nth mt 1) [2.0, 5.0])

# outerProduct
@op (outerProduct [1.0, 2.0] [3.0, 4.0])
assert (eq (nth op 0) [3.0, 4.0])
assert (eq (nth op 1) [6.0, 8.0])

# matAdd
@ma [[1.0, 2.0], [3.0, 4.0]]
@mb [[5.0, 6.0], [7.0, 8.0]]
assert (eq (matAdd ma mb) [[6.0, 8.0], [10.0, 12.0]])

# matScale
assert (eq (matScale 2.0 [[1.0, 2.0]]) [[2.0, 4.0]])

# matZeros
@mz (matZeros 2 3)
assert (eq (len mz) 2)
assert (eq (nth mz 0) [0.0, 0.0, 0.0])

# matRandom
@mr (matRandom 2 3 1.0)
assert (eq (len mr) 2)
assert (eq (len (nth mr 0)) 3)

true
