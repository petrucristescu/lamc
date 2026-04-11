# Test comparison operators (#47)

~(
    # gt
    assert (gt 5 3)
    assert (not (gt 3 5))
    assert (not (gt 3 3))
    assert (gt 5.0 3.0)
    assert (gt "b" "a")

    # lt
    assert (lt 3 5)
    assert (not (lt 5 3))
    assert (not (lt 3 3))
    assert (lt 2.0 3.0)

    # gte
    assert (gte 5 3)
    assert (gte 3 3)
    assert (not (gte 2 3))

    # lte
    assert (lte 3 5)
    assert (lte 3 3)
    assert (not (lte 5 3))

    # Mixed int/float
    assert (gt 5 3.0)
    assert (lt 2.0 5)

    # Use in filter
    @nums [1, 2, 3, 4, 5]
    assert (eq (filter (|>x. gt x 3) nums) [4, 5])
    assert (eq (filter (|>x. lte x 2) nums) [1, 2])
)
