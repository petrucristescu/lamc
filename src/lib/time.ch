# Time Library
# Native primitives: now, timeMs, year, month, day, hour, minute, second,
#   dayOfWeek, diffTime

# Check if a year is a leap year
~isLeapYear y (
    eq (y - (y / 4) * 4) 0
        (|>_. eq (y - (y / 100) * 100) 0
            (|>_. eq (y - (y / 400) * 400) 0
                (|>_. true)
                (|>_. false)
            0)
            (|>_. true)
        0)
        (|>_. false)
    0
)
