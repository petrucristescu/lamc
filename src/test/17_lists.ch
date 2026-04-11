# Test native list literals and list operations (Approach B)

@nums [1, 2, 3, 4, 5]
@empty []
@strs ["hello", "world"]

~(
    # Basic operations
    assert (eq (len nums) 5)
    assert (eq (len empty) 0)
    assert (eq (head nums) 1)
    assert (eq (tail nums) [2, 3, 4, 5])
    assert (empty empty)
    assert (not (empty nums))

    # cons
    assert (eq (cons 0 nums) [0, 1, 2, 3, 4, 5])
    assert (eq (cons 1 []) [1])

    # nth
    assert (eq (nth nums 0) 1)
    assert (eq (nth nums 4) 5)

    # reverse
    assert (eq (reverse nums) [5, 4, 3, 2, 1])
    assert (eq (reverse empty) [])

    # range
    assert (eq (range 1 6) [1, 2, 3, 4, 5])
    assert (eq (range 0 0) [])

    # map
    assert (eq (map (|>x. x * 2) nums) [2, 4, 6, 8, 10])
    assert (eq (map (|>x. x + 1) [10, 20]) [11, 21])

    # filter — keep elements > 3
    @big (filter (|>x. not (eq x 1) ) [1, 2, 3])
    assert (eq big [2, 3])

    # foldl — sum
    assert (eq (foldl (|>acc. |>x. acc + x) 0 nums) 15)

    # foldr — build reversed
    assert (eq (foldr (|>x. |>acc. cons x acc) [] nums) [1, 2, 3, 4, 5])

    # strings in lists
    assert (eq (head strs) "hello")
    assert (eq (len strs) 2)
)
