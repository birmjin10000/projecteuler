expansions =
    (3,2):map (\(n, d) -> (n+2*d,n+d)) expansions

hasLongerNumerator (n,d) =
    (length.show$n) > (length.show$d)

main =
    print $ length.filter hasLongerNumerator$take 1000 expansions
