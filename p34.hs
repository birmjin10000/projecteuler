import Data.Char

factorial = [1, 1, 2, 6, 24, 120, 720, 5040, 40320, 362880]

digit_factorial_sum n = 
	let digits = map digitToInt (show n)
	in sum $ map (factorial!!) digits

isDigitFactorials n = n == digit_factorial_sum n

-- 2!!9 * 7 = 2540160, still 7-digit number. so upper limit is 999999.
getSumOfDigitFactorials =
	let digit_factorials = [x|x<-[10..999999], isDigitFactorials x]
	in sum $ digit_factorials
