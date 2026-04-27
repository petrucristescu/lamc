# Test arithmetic operators

@v1 4
@v3 4l
@v4 7l
@v5 2.4

~add x,y x + y
~sub x,y x - y
~mul x,y x * y

assert (eq (add 2 4) 6)
assert (eq (add 2 v1) 6)
assert (eq (add 2 3) 5)
assert (eq (add v3 v4) 11)
assert (eq (add (sub 5 2) 3) 6)
assert (eq (add 2 (sub 5 3)) 4)

assert (eq (sub 2 3) (0 - 1))
assert (eq (sub (add 2 3) 1) 4)

assert (eq (mul 2 2) 4)
assert (eq (mul v3 v4) 28)

assert (eq (2 + 100) 102)
