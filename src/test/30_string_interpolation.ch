# Test string interpolation helpers (#48)

# str: concatenate list of values into string
@name "Alice"
@age 30
assert (eq (str ["Hello ", name, ", age ", age]) "Hello Alice, age 30")
assert (eq (str [1, " + ", 2, " = ", 3]) "1 + 2 = 3")
assert (eq (str []) "")

# join: join list with separator
assert (eq (join ", " ["a", "b", "c"]) "a, b, c")
assert (eq (join " " [1, 2, 3]) "1 2 3")
assert (eq (join "-" []) "")

# Practical: build a response
@user "Bob"
@score 95
assert (eq (str [user, " scored ", score, "%"]) "Bob scored 95%")
