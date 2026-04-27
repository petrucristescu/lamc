import "pgm"
import "vector"

~inRect x,y,r (and (gte x (nth r 0)) (and (lte x (nth r 2)) (and (gte y (nth r 1)) (lte y (nth r 3)))))

~inAnyRect x,y,rects (matchList rects (|>_. false) (|>r. |>rest. matchBool (inRect x y r) true (inAnyRect x y rest)))

~pixelAt i,rects (
    @y (floor (i / 32))
    @x (i - y * 32)
    matchBool (inAnyRect x y rects) 1.0 0.0
)

~makeImage rects (map (|>i. pixelAt i rects) (range 0 1024))

# Digit 0
@digit0 (makeImage [[8, 4, 24, 7], [8, 25, 24, 28], [8, 4, 11, 28], [21, 4, 24, 28]])

assert (eq (len digit0) 1024)
writePgm "examples/digits/test/digit_0.pgm" 32 32 digit0

true
