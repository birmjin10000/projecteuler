import Data.List

rmDuplicate = map head.group.sort
hasDistinctDigit str = length (rmDuplicate str) == length str

multiplyFixedWidth multiplier base n =
    if (n*multiplier < base)
        then ('0':(show (n*multiplier)))
        else show (n*multiplier) 

canBeMultiple multiplier range str =
    let digitSet = ['0'..'9'] \\ str
    in [(head m):str | m <- map (\x -> multiplyFixedWidth multiplier 100 x) range, elem (head m) digitSet, tail m == take 2 str]

canBe2Multiple = canBeMultiple 2 [1..499]
canBe3Multiple = canBeMultiple 3 [1..333]
canBe5Multiple = canBeMultiple 5 [1..199]
canBe7Multiple = canBeMultiple 7 [1..142]
canBe11Multiple = canBeMultiple 11 [1..90]
canBe13Multiple = canBeMultiple 13 [1..76]

threeDigit17Multiples =
    filter hasDistinctDigit $ map (\x -> multiplyFixedWidth 17 100 x) [1..58]

completePandigital str =
    let firstDigit = head $ ['0'..'9'] \\ str
    in firstDigit:str

divBy13 = concat.filter (/=[]).map canBe13Multiple
divBy11 = concat.filter (/=[]).map canBe11Multiple
divBy7 = concat.filter (/=[]).map canBe7Multiple
divBy5 = concat.filter (/=[]).map canBe5Multiple
divBy3 = concat.filter (/=[]).map canBe3Multiple
divBy2 = concat.filter (/=[]).map canBe2Multiple

from17to2Dividables = (divBy2.divBy3.divBy5.divBy7.divBy11.divBy13) threeDigit17Multiples
main = print.sum.map (read.completePandigital) $ from17to2Dividables

