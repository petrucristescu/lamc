# Test Church-style eliminators (Option B — matching as function application)

# matchList: eliminates a list into empty/cons cases
@nums [1, 2, 3]

# Sum using matchList recursion
~sumList l (matchList l
    (|>_. 0)
    (|>h. |>t. h + sumList t))
assert (eq (sumList nums) 6)
assert (eq (sumList []) 0)

# Length using matchList
~listLen l (matchList l
    (|>_. 0)
    (|>_. |>t. 1 + listLen t))
assert (eq (listLen nums) 3)
assert (eq (listLen []) 0)

# matchBool: eliminates a boolean
@result (matchBool true "yes" "no")
assert (eq result "yes")

@result2 (matchBool false "yes" "no")
assert (eq result2 "no")

# Compose: filter using matchList + matchBool
~myFilter f,l (matchList l
    (|>_. [])
    (|>h. |>t. matchBool (f h)
        (cons h (myFilter f t))
        (myFilter f t)))
assert (eq (myFilter (|>x. eq x 2) [1, 2, 3, 2]) [2, 2])
