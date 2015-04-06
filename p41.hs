import Data.List

panDigital_primes:: Int -> [Int]
panDigital_primes n =
    let seeds = take n [1..9]
    in filter isPrime $ map (read.concatMap show) (permutations seeds)

isPrime n =
	let divisors = [d|d<-[3,5..(floor.sqrt $ fromIntegral n)], n `mod` d == 0]
	in (odd n) && (length divisors == 0)


