# String Library
# Native primitives: length, concat, substring, uppercase, lowercase,
#   trim, charAt, indexOf, startsWith, endsWith, replace, toString,
#   str, join

# Check if string is empty
~isEmpty s (eq (length s) 0)

# Check if string contains a substring
~contains s,sub (not (eq (indexOf s sub) (0 - 1)))
