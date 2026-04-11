# Test Church-encoded lists (Approach A — pure lambda calculus)

# Build a Church list: [1, 2, 3]
@mylist (church_cons 1 (church_cons 2 (church_cons 3 church_nil)))

~(
    # Church head
    assert (eq (church_head mylist) 1)

    # Church sum via fold
    assert (eq (church_sum mylist) 6)

    # Church length
    assert (eq (church_length mylist) 3)

    # Church fold directly
    assert (eq (church_fold (|>x. |>acc. x + acc) 0 mylist) 6)

    # Church map + sum: double each element then sum
    assert (eq (church_sum (church_map (|>x. x * 2) mylist)) 12)

    # Empty list
    assert (eq (church_length church_nil) 0)
    assert (eq (church_sum church_nil) 0)
)
