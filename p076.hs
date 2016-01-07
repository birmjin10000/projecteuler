import Data.Array
-- naive, brute-force solution
countCombi n = f n [1..(n-1)]

f 0 _ = 1
f _ [] = 0
f n xs'@(x:xs)
    | n < x = 0
    | otherwise = f n xs + f (n - x) xs'

-- Solution using Dynamic Programming technique.
type Amount = Integer
countSummation:: Amount -> Integer
countSummation amount = getCount amount numbers
  where
    numbers = [1..amount-1]
    numLength = length numbers
    getCount _ [] = 0
    getCount amount' numbers'
      | amount' < 0 = 0
      | amount' == 0 = 1
      | otherwise = arrayCount ! (amount', length numbers' - 1)
    arrayCount =
      array ((0,0),(amount, numLength - 1)) [((a,i), calcCount a i)|a<-[0..amount], i<-[0..numLength - 1]]
    calcCount a i = getCount (a-number) (take (i+1) numbers) + getCount a (take i numbers)
      where number = numbers !! i

