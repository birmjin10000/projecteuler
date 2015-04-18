import Data.Char
import qualified Data.Set as S
import Data.List.Split
import System.IO

triangleNumber n = n*(n+1) `div` 2

triangleNumber100 = S.fromList $ map triangleNumber [1..99]
charPoint ch = ord ch - 64
isTriangularWord word = S.member (sum $ map charPoint word) triangleNumber100 

main = do
    wordsFile <- readFile "p042_words.txt"
    print $ length $ filter (==True) $ map isTriangularWord (map (\e -> init (tail e)) (splitOn "," wordsFile))
