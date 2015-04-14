import Data.List

digits3 = [100..166]
digits4 = [1000..1666]
digits5 = [10000..16666]
digits6 = [100000..166666]
digits7 = [1000000..1666666]
digits8 = [10000000..16666666]
digits9 = [100000000..166666666]

containsSameDigits digits n =
    length (show n \\ digits) == 0

is6timesSameDigits n =
    let multiples = sequence [(*)2, (*)3, (*)4, (*)5, (*)6] n
    in (length.(dropWhile (==True)) $ map (containsSameDigits (show n)) multiples) == 0

sixTimesSameDigit = foldl (\acc e -> if (is6timesSameDigits e) then (e:acc) else acc) []

main = do
    print $ head.(dropWhile (==[])) $ map sixTimesSameDigit [digits3, digits4, digits5, digits6, digits7, digits8, digits9]
