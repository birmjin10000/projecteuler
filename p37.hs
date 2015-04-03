-- projecteuler.net p37.
import Data.Char

isPrimeOver2:: Integer -> Bool
isPrimeOver2 n = 
    length divisors == 0
	where divisors = [x | x <- [2..floor.sqrt $ (fromIntegral n)], n `mod` x == 0]

primes = 2:[x|x<-[3,5..], isPrimeOver2 x]
overTwoDigitsPrimes = drop 4 primes

isPrime n = n == 2 || n > 1 && odd n && isPrimeOver2 n

isTruncatedPrime n =
    --(foldr (&&) True (map (odd.digitToInt) (show n))) &&
    (length (filter (not.isPrime) truncatedNumbers1) == 0) && 
    (length (filter (not.isPrime) truncatedNumbers2) == 0)
    where number_width = length (show n) - 1
          truncatedNumberString1 = sequence (scanl1 (.) (replicate number_width tail)) (show n)
          truncatedNumberString2 = sequence (map take [1..number_width]) (show n)
          truncatedNumbers1 = map read truncatedNumberString1
          truncatedNumbers2 = map read truncatedNumberString2

truncatedPrimes = [x|x<-overTwoDigitsPrimes, isTruncatedPrime x] 
