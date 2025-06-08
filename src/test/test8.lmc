@b_v1 false
@b_v2 true

~foo (
    print "foo called"
)

~bar (
    print "bar called"
)

~choose_fn x,y (
    eq x y foo bar
)

~(
    print v1
    print v2
    print(eq 1 1 foo bar)
    print(eq 1 2 foo bar)

    print choose_fn 1 1
    print choose_fn 1 2

    print choose_fn v1 v1
    print choose_fn v1 v2
)