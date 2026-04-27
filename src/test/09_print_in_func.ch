# Test function application with multiple args
@v1 9
@v2 8

~add x,y x + y
~sub x,y x - y

assert (eq (add 2 3) 5)
assert (eq (sub 5 3) 2)
assert (eq v1 9)
assert (eq v2 8)
true
