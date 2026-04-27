# Load trained weights
@weightsJson (readFile "examples/digits/weights.json")
@net (fromJson weightsJson)

# Read input image (hardcoded path - change to test different images)
@imgPath "examples/digits/data/test/0_0.pgm"
@img (readPgm imgPath)
@pixels (get img "pixels")

# Run prediction
@acts (forward net pixels)
@output (get acts "output")
@prediction (argmax output)

str ["Image: ", imgPath]
str ["Predicted digit: ", prediction]
str ["Confidence scores: ", output]
