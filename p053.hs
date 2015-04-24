import Control.Applicative
combi n r =
		product (take r [n,n-1..]) `div` product (take r [1..])

combinations100 = combi <$> [10..100] <*> [4..97]
countOver1M =
    length $ filter (\e -> (length.show$e) > 6) combinations100
