# Test with a tiny random input (4 pixels instead of 1024 for speed)
# We can't test the full 1024-input network here due to performance,
# but we can test the building blocks work together

# Test oneHot
assert (eq (oneHot 3 10) [0.0, 0.0, 0.0, 1.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0])

# Test xavierScale produces reasonable values
@xs (xavierScale 100 50)
assert (gt xs 0.0)
assert (lt xs 1.0)

# Test softmax on a small vector
@sm (softmax [1.0, 2.0, 3.0])
assert (lt (abs ((vecSum sm) - 1.0)) 0.0001)
assert (eq (argmax sm) 2)

# Test crossEntropy
@ce (crossEntropy [0.1, 0.2, 0.7] 2)
assert (lt ce 1.0)
