import Data.List

sumOfProperDivisors::Integral a => a -> a
sumOfProperDivisors n = 
    let firstHalf = [x|x<- [2..(floor.sqrt $ (fromIntegral n))], n `mod` x == 0]
        secondHalf = map (div n) firstHalf
    in 1 + (sum $ nub (firstHalf ++ secondHalf))

isAmicableNumber n =
    n == sumOfProperDivisors sumX && n /= sumX
    where sumX = sumOfProperDivisors n

getAmicableNumbers acc [] = acc
getAmicableNumbers acc (x:xs) =
    if (isAmicableNumber x) then getAmicableNumbers (x:acc) xs
    else getAmicableNumbers acc xs
