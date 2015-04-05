import Data.List.Split
import Data.List
import System.IO
import Data.Char

name_score name = foldr (\x y -> (ord x - 64) + y) 0 name 

main = do
    nameFile <- readFile "p022_names.txt"
    let names = sort $ map (\e -> init.tail $ e) $ splitOn "," nameFile
    let indexed_names = zip [1..] names
    print $ sum $ map (\(i, name) -> i * (name_score name)) indexed_names
