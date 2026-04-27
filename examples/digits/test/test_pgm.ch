import "pgm"

# Write a small 2x2 PGM image, then read it back
@testPath "examples/digits/test/test_output.pgm"
@pixels [1.0, 0.5, 0.0, 0.75]
writePgm testPath 2 2 pixels

# Read it back
@img (readPgm testPath)
assert (eq (get img "width") 2)
assert (eq (get img "height") 2)

@readPixels (get img "pixels")
assert (eq (len readPixels) 4)

# Check values are approximately correct (rounding to 255 and back)
assert (lt (abs ((nth readPixels 0) - 1.0)) 0.01)
assert (lt (abs ((nth readPixels 1) - 0.5)) 0.01)
assert (lt (abs ((nth readPixels 2) - 0.0)) 0.01)

# Clean up
deleteFile testPath

true
