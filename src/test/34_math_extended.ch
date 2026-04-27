# Test exp, log, tanh primitives (#55)

# exp
assert (eq (round (exp 0)) 1)
assert (eq (round (exp 1)) 3)

# log
assert (eq (round (log 1)) 0)
assert (lt (abs ((log (exp 1.0)) - 1.0)) 0.0001)

# tanh
assert (eq (tanh 0) 0.0)
assert (lt (abs ((tanh 1.0) - 0.7615941559)) 0.0001)
assert (lt (tanh 100) 1.001)
assert (gt (tanh 100) 0.999)

# exp and log inverse
@x 3.14
assert (lt (abs ((log (exp x)) - x)) 0.0001)

true
