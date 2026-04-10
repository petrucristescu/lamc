# Testing operators with ~ prefix syntax
~import operators

~(
    print (false "yes" "no")

    # Test boolean operators
    print (not false "yes" "no")
    print (not true "yes" "no")

    # Test AND operator
    print (and true true "yes" "no")
    print (and true false "yes" "no")
    print (and false true "yes" "no")

    # Test OR operator
    print (or false false "yes" "no")
    print (or true false "yes" "no")
    print (or false true "yes" "no")

    # Test chained boolean operations
    print (not (and true false) "Correctly not-ed" "Should not see this")
    print (or false true "yes" "no")

    # Test complex boolean expression
    print (and (or true false) (not false) "Complex expression works" "Should not see this")
)
