import Data.List

concatenateMultiple n =
    (show n) ++ (show (n*2))
isPandigital n =
   (length $ intersect "123456789" (concatenateMultiple n)) == 9

concatMultiples = findIndices (==True) $ map isPandigital [9213..9876]

main = print $ concatenateMultiple ([9213..9876]!!(maximum concatMultiples)) 
