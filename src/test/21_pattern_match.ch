# Test pattern matching (Option A — match expression)

~(
    # Match on integers
    @x 42
    @r1 (match x
        | 0 -> "zero"
        | 42 -> "answer"
        | _ -> "other")
    assert (eq r1 "answer")

    # Match on booleans
    @b true
    @r2 (match b
        | true -> "yes"
        | false -> "no")
    assert (eq r2 "yes")

    # Match on strings
    @s "hello"
    @r3 (match s
        | "hello" -> 1
        | "world" -> 2
        | _ -> 0)
    assert (eq r3 1)

    # Match with variable binding
    @y 99
    @r4 (match y
        | n -> n + 1)
    assert (eq r4 100)

    # Match on lists — empty
    @r5 (match []
        | [] -> "empty"
        | _ -> "nonempty")
    assert (eq r5 "empty")

    # Match on lists — exact pattern
    @nums [1, 2, 3]
    @r6 (match nums
        | [1, 2, 3] -> "exact"
        | _ -> "other")
    assert (eq r6 "exact")

    # Match with cons destructuring (head :: tail)
    @r7 (match nums
        | h :: t -> h)
    assert (eq r7 1)

    @r8 (match nums
        | h :: t -> t)
    assert (eq r8 [2, 3])

    # Wildcard in patterns
    @r9 (match [10, 20, 30]
        | [_, second, _] -> second)
    assert (eq r9 20)

    # Nested match — factorial
    ~fact n (match n
        | 0 -> 1
        | x -> x * fact (x - 1))
    assert (eq (fact 5) 120)
)
