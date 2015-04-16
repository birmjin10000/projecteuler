import Data.List

consecutiveFour = consecutiveFour' [10..]

consecutiveFour' target =
    if allHas4Primes then head target
    else consecutiveFour' (tail target)
    where allHas4Primes = and $ take 4 $ map hasFourPrimes target 


hasFourPrimes n = length (primeFactors n) == 4

primeFactors n
    | (odd n) = primeFactors' n [] candidates 
    | otherwise = primeFactors' (divideUntil n 2) [2] candidates
    where candidates = [3,5..(floor.sqrt$fromIntegral n)]

primeFactors' 1 factors _ = factors
primeFactors' n factors [] = n:factors
primeFactors' n factors (c:cs) =
    if (n `mod` c == 0) then primeFactors' (divideUntil n c) (c:factors) cs
    else primeFactors' n factors cs
    
divideUntil n divisor =
    if (n `mod` divisor == 0) then divideUntil (n `div` divisor) divisor
    else n
