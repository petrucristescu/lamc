# Test random number generation (#56)

@r1 (random 0)
@r2 (random 0)

# Both should be in [0, 1)
assert (gte r1 0.0)
assert (lt r1 1.0)
assert (gte r2 0.0)
assert (lt r2 1.0)

true
