import Data.List.Split

mergePath' accPathSum currentPath 0 pathLength = 
	mergePath' accPathSum (((head accPathSum) + (head currentPath)):(tail currentPath)) 1 pathLength

mergePath' accPathSum currentPath position pathLength =
    if (position == pathLength) then currentPath
    else let minPath = min (currentPath!!(position-1)) (accPathSum!!position)
         in mergePath' accPathSum ((take position currentPath) ++ ((minPath + (currentPath!!position)):(drop (position+1) currentPath))) (position+1) pathLength

mergePath accPathSum currentPath =
    if (length accPathSum == 0) then scanl1 (+) currentPath
    else mergePath' accPathSum currentPath 0 (length currentPath)

minimalPathSum accPathSum grid =
    foldl mergePath accPathSum grid 

splitComma = map (splitOn ",")
makePathGrid grid = map (\line -> map (\n -> read n::Int) line) (splitComma grid)

main = do
    gridFile <- readFile "p081_matrix.txt"
    let pathGrid = makePathGrid (lines gridFile)
    print $ last $ minimalPathSum [] pathGrid
