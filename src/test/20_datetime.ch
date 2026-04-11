# Test date/time standard library functions

~(
    # now returns a positive timestamp
    @ts (now 0)
    assert (not (eq ts 0))

    # year/month/day extract components
    @y (year ts)
    assert (not (eq y 0))

    # diffTime
    assert (eq (diffTime 100 50) 50)
    assert (eq (diffTime 50 100) 0 - 50)

    # dayOfWeek returns 0-6
    @dow (dayOfWeek ts)
    assert (not (eq dow 0 - 1))

    # extractors on a known timestamp (Unix epoch = Jan 1 1970 00:00:00 UTC)
    # Note: these depend on timezone, so we just test they return integers
    @h (hour 0)
    @m (minute 0)
    @s (second 0)
    assert (eq m 0)
    assert (eq s 0)
)
