import Data.List.Split

main = do
  file <- readFile "p099_base_exp.txt"
  let listOfBaseExp = zip [1..] $ map (map (\s -> read s::Float) . (splitOn ",")) . lines $ file
  print $ foldl (\(ia,as@(a:[ae])) (ix,xs@(x:[xe])) ->
                                   if (ae * log a > xe * log x)
                                   then (ia,as)
                                   else (ix,xs)) (0,[1,1]) listOfBaseExp
