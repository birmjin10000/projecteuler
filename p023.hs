import Data.List 
import qualified Data.Set as Set

divisorsBottomHalf n = 1:[d|d<-[2..(floor.sqrt$fromIntegral n)], n `mod` d == 0]
divisorsTopHalf n = map (div n) (tail $ divisorsBottomHalf n)
divisors n = nub $ (divisorsBottomHalf n) ++ (divisorsTopHalf n)
sumOfDivisors n = sum $ divisors n
isAbundantNumber n = (sumOfDivisors n) > n
abundantNumbers = filter isAbundantNumber [12..28123]

numberByAddingTwoSameAbundantNumbers = map (*2) $ filter (<=14062) abundantNumbers
firstFiltering = [12..28123] \\ numberByAddingTwoSameAbundantNumbers 

canBeSumOfTwo numbers firstIndex n =
    let firstOne = Set.elemAt firstIndex numbers
    in
      if (n<=firstOne || (n-firstOne) < (Set.elemAt (firstIndex+1) numbers)) then False
      else
        let matchingPair = Set.lookupIndex (n-firstOne) numbers
        in if (matchingPair == Nothing) then canBeSumOfTwo numbers (firstIndex+1) n
           else True

numberByAddingTwoDifferentAbundantNumbers =
    filter (canBeSumOfTwo (Set.fromList abundantNumbers) 0) firstFiltering

main = print $ sum [1..28123] - (sum numberByAddingTwoSameAbundantNumbers + sum numberByAddingTwoDifferentAbundantNumbers)

