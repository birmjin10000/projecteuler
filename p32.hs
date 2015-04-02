-- projecteuler.net Problem No.32
import Data.List

rawTripleSequence = [[x*y,x,y]|x<- [1..98], y<-[x+1..4987]]

pred1::[Integer] -> Bool
pred1 = (notElem '0') . concat . (map show)

isPan xs = (length.show) xs == (length.nub.show) xs
pred2::[Integer] -> Bool
pred2 = (notElem False) . (map isPan)

pred3 xs =
    (sum $ map length ((\xs -> zipWith intersect xs (tail xs ++ [head xs])) $ map show xs)) == 0

pred4 xs = (sum $ map length $ map show xs) == 9

sumOfPanDigitalProducts =
    sum.nub.(map head).(filter pred1).(filter pred2).(filter pred3).(filter pred4) $ rawTripleSequence

