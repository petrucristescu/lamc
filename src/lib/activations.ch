# Activation functions for neural network

# Sigmoid: 1 / (1 + exp(-x))
~sigmoid x (1.0 / (1.0 + exp (0.0 - x)))

# Sigmoid derivative: sigmoid(x) * (1 - sigmoid(x))
~sigmoidDeriv x (
    @s (sigmoid x)
    s * (1.0 - s)
)

# ReLU: max(0, x)
~relu x (matchBool (gt x 0.0) x 0.0)

# ReLU derivative: 1 if x > 0, else 0
~reluDeriv x (matchBool (gt x 0.0) 1.0 0.0)

# Softmax: normalize exponentials
~softmax vec (
    @maxVal (foldl (|>a. |>b. matchBool (gt a b) a b) (head vec) (tail vec))
    @exps (map (|>x. exp (x - maxVal)) vec)
    @total (vecSum exps)
    map (|>x. x / total) exps
)
