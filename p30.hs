-- projecteuler.net p30. Digit fifith powers.
import Data.Char

toDigits n = map digitToInt (show n)
isDigitPowers number = (sum $ map (^5) (toDigits number)) == number

digitPowerSum = 
    (filter (\n -> n /= 0)).(map (\n -> if isDigitPowers n then n else 0)) 

main = do
    let list = digitPowerSum [2..(9^5 * 6)]
    print list
    print $ sum list
