import Data.List

concatenateMultiple n =
    (show n) ++ (show (n*2))
isPandigital n =
   (length $ intersect "123456789" (concatenateMultiple n)) == 9

maxConcatMultiple = 
    [9212..9871]!!(maximum $ findIndices (==True) $ map isPandigital [9212..9871])
