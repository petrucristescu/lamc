# Sequence helper
~seq a,b b

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
    ~loadDigit d,acc (matchBool (gt d 9) acc (
        seq (print (str ["Loading digit ", d, "..."]))
        (loadDigit (d + 1) (append acc (loadDigitSamples d 15 dir)))
    ))
    loadDigit 0 []
)

# Compute loss on a single sample
~sampleLoss net,sample (
    @acts (forward net (get sample "input"))
    @output (get acts "output")
    crossEntropy output (get sample "label")
)

# Compute average loss over all samples
~avgLoss net,samples (
    @totalLoss (foldl (|>acc. |>s. acc + (sampleLoss net s)) 0.0 samples)
    totalLoss / (toFloat (len samples))
)

# Train for one epoch with per-sample progress
~trainEpochVerbose net,samples,lr,sampleIdx (matchList samples
    (|>_. net)
    (|>s. |>rest.
        @net2 (trainOne net (get s "input") (get s "label") lr)
        seq (print (str ["  sample ", sampleIdx, "/", sampleIdx + (len rest), " label=", (get s "label")]))
        (trainEpochVerbose net2 rest lr (sampleIdx + 1))
    ))

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
~trainLoop net,samples,epoch,maxEpochs,lr (match (gt epoch maxEpochs)
    | true -> net
    | false ->
        seq (print (str ["=== Epoch ", epoch, "/", maxEpochs, " ==="]))
        (@net2 (trainEpochVerbose net samples lr 1)
        @acc (computeAccuracy net2 samples)
        @loss (avgLoss net2 samples)
        seq (print (str ["  accuracy: ", acc, "  loss: ", loss]))
        (trainLoop net2 samples (epoch + 1) maxEpochs lr)))

# === MAIN ===

seq (print "=== Churing Neural Network Training ===") 0
seq (print "Architecture: 1024 -> 128 (ReLU) -> 64 (ReLU) -> 10 (Softmax)") 0

# Load training data
@samples (loadAllSamples "examples/digits/data/train")
seq (print (str ["Loaded ", (len samples), " training samples"])) 0

# Initialize network
seq (print "Initializing network (140,106 parameters)...") 0
@net (initNetwork 0)
seq (print "Network initialized with Xavier weights") 0

# Train for 3 epochs with learning rate 0.01
@trained (trainLoop net samples 1 3 0.01)

# Compute final accuracy
@finalAcc (computeAccuracy trained samples)
seq (print (str ["=== Final training accuracy: ", finalAcc, " ==="])) 0

# Save weights as JSON
seq (print "Saving weights to examples/digits/weights.json...") 0
writeFile "examples/digits/weights.json" (toJson trained)
seq (print "Done!") 0
