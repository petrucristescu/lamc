# Load training data from PGM files
# Filename format: {digit}_{idx}.pgm
~loadSample path (
    @parts (split path "/")
    @filename (nth parts ((len parts) - 1))
    @label (toInt (nth (split filename "_") 0))
    @img (readPgm path)
    @pixels (get img "pixels")
    {input: pixels, label: label}
)

# Load all samples for a digit from a directory
~loadDigitSamples digit,count,dir (
    ~loadN i,acc (matchBool (gte i count) acc (
        @path (str [dir, "/", digit, "_", i, ".pgm"])
        matchBool (fileExists path)
            (loadN (i + 1) (cons (loadSample path) acc))
            (loadN (i + 1) acc)
    ))
    loadN 0 []
)

# Load all training data (digits 0-9, up to 15 samples each)
~loadAllSamples dir (
    ~loadDigit d,acc (matchBool (gt d 9) acc (loadDigit (d + 1) (append acc (loadDigitSamples d 15 dir))))
    loadDigit 0 []
)

# Train for one epoch: iterate over all samples
~trainEpoch net,samples,lr (foldl (|>net. |>s. trainOne net (get s "input") (get s "label") lr) net samples)

# Compute accuracy on a set of samples
~computeAccuracy net,samples (
    @correct (foldl (|>acc. |>s.
        @pred (predict net (get s "input"))
        matchBool (eq pred (get s "label")) (acc + 1) acc
    ) 0 samples)
    @total (len samples)
    (toFloat correct) / (toFloat total)
)

# Main training loop
~trainLoop net,samples,epoch,maxEpochs,lr (matchBool (gt epoch maxEpochs) net (
    @net2 (trainEpoch net samples lr)
    @acc (computeAccuracy net2 samples)
    @dummy (str ["Epoch ", epoch, " - accuracy: ", acc])
    trainLoop net2 samples (epoch + 1) maxEpochs lr
))

# === MAIN ===

# Load training data
@samples (loadAllSamples "examples/digits/data/train")
@numSamples (len samples)
str ["Loaded ", numSamples, " training samples"]

# Initialize network
@net (initNetwork 0)

# Train for 5 epochs with learning rate 0.01
@trained (trainLoop net samples 1 5 0.01)

# Compute final accuracy
@finalAcc (computeAccuracy trained samples)
str ["Final training accuracy: ", finalAcc]

# Save weights as JSON
writeFile "examples/digits/weights.json" (toJson trained)
