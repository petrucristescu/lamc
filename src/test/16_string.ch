# Test string standard library functions
import "string"

~(
    # length
    assert (eq (length "hello") 5)
    assert (eq (length "") 0)

    # concat
    assert (eq (concat "hello" " world") "hello world")
    assert (eq (concat "" "abc") "abc")

    # substring
    assert (eq (substring "hello world" 0 5) "hello")
    assert (eq (substring "hello world" 6 5) "world")

    # uppercase, lowercase
    assert (eq (uppercase "hello") "HELLO")
    assert (eq (lowercase "HELLO") "hello")

    # trim
    assert (eq (trim "  hello  ") "hello")
    assert (eq (trim "abc") "abc")

    # charAt
    assert (eq (charAt "hello" 0) "h")
    assert (eq (charAt "hello" 4) "o")

    # indexOf
    assert (eq (indexOf "hello world" "world") 6)
    assert (eq (indexOf "hello" "xyz") 0 - 1)
    assert (eq (indexOf "hello" "ell") 1)

    # startsWith, endsWith
    assert (startsWith "hello world" "hello")
    assert (not (startsWith "hello" "world"))
    assert (endsWith "hello world" "world")
    assert (not (endsWith "hello" "world"))

    # replace
    assert (eq (replace "hello world" "world" "there") "hello there")
    assert (eq (replace "aaa" "a" "bb") "bbbbbb")

    # toString
    assert (eq (toString 42) "42")
    assert (eq (toString "hello") "hello")
    assert (eq (toString true) "true")
)
