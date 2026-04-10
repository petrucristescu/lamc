# Operator precedence: * and / bind tighter than + and -
# Left associativity: 5 - 3 - 1 = (5 - 3) - 1 = 1

~(
    assert (eq (2 * 3 + 4) 10)
    assert (eq (4 + 2 * 3) 10)
    assert (eq (10 - 3 * 2) 4)
    assert (eq (5 - 3 - 1) 1)
    assert (eq (2 + 3 + 4) 9)
    assert (eq (2 + 3) 5)
    assert (eq (10 - 2 + 3) 11)
)
