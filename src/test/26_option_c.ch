# Test Option C: no ~() main block, last value is the program result

~double n (n * 2)
~fact n (match n | 0 -> 1 | x -> x * fact (x - 1))

# Intermediate expressions execute (assertions)
assert (eq (double 5) 10)
assert (eq (fact 5) 120)

# Last expression is the program's return value (auto-printed)
fact 10
