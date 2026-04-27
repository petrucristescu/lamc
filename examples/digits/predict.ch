# Sequence helper
~seq a,b b

# Load trained weights
seq (print "Loading weights...") 0
@weightsJson (readFile "examples/digits/weights.json")
@net (fromJson weightsJson)
seq (print "Weights loaded.") 0

# Read input image (hardcoded path - change to test different images)
@imgPath "examples/digits/data/test/0_0.pgm"
seq (print (str ["Image: ", imgPath])) 0
@img (readPgm imgPath)
@pixels (get img "pixels")

# Run prediction
@acts (forward net pixels)
@output (get acts "output")
@prediction (argmax output)

print (str ["Predicted digit: ", prediction])
print (str ["Confidence: ", output])
