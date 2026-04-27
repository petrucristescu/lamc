# oneHot
assert (eq (oneHot 0 3) [1.0, 0.0, 0.0])
assert (eq (oneHot 2 3) [0.0, 0.0, 1.0])
assert (eq (len (oneHot 5 10)) 10)

# crossEntropy: perfect prediction -> low loss
@pred [0.0001, 0.0001, 0.9998]
@loss (crossEntropy pred 2)
assert (lt loss 0.001)

# crossEntropy: wrong prediction -> high loss
@loss2 (crossEntropy pred 0)
assert (gt loss2 5.0)
