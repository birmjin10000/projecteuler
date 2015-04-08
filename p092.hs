import Data.Char

squareDigitSum n =
	sum $ map ((^2).digitToInt) (show n)

determineEndingDigits n
	| (n == 1 || n == 89) = n
	| otherwise = determineEndingDigits (squareDigitSum n)

-- squareDigitSum 9999999 = 81 * 7 = 567
dictionaryForTenMillions = map determineEndingDigits [1..567]

countEnding89 n count
	| (dictionaryForTenMillions!!(n-1) == 89) = count + 1
	| otherwise = count

main = do
	let count568To10Millions = foldr countEnding89 0 (map squareDigitSum [568..10000000])
	print $ count568To10Millions + (length $ filter (==89) dictionaryForTenMillions)
