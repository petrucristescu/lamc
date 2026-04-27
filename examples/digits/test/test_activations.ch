import "activations"

# sigmoid(0) = 0.5
assert (eq (sigmoid 0.0) 0.5)

# sigmoid of large positive -> close to 1
assert (gt (sigmoid 10.0) 0.999)

# sigmoid of large negative -> close to 0
assert (lt (sigmoid (0.0 - 10.0)) 0.001)

# sigmoidDeriv at 0 = 0.25
assert (eq (sigmoidDeriv 0.0) 0.25)

# relu
assert (eq (relu 5.0) 5.0)
assert (eq (relu (0.0 - 3.0)) 0.0)
assert (eq (relu 0.0) 0.0)

# reluDeriv
assert (eq (reluDeriv 5.0) 1.0)
assert (eq (reluDeriv (0.0 - 1.0)) 0.0)

# softmax: should sum to 1.0
@sm (softmax [1.0, 2.0, 3.0])
assert (eq (len sm) 3)
@smSum (vecSum sm)
assert (lt (abs (smSum - 1.0)) 0.0001)

# softmax: largest input -> largest probability
assert (gt (nth sm 2) (nth sm 1))
assert (gt (nth sm 1) (nth sm 0))

true
