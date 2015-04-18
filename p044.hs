import qualified Data.Set as S

pentagonalNumber n = n*(3*n - 1) `div` 2
pNumbers10k = map pentagonalNumber [1..9999]
pNumbers10kSet = S.fromList pNumbers10k

isBothSumAndDiffArePentagonal (p1, p2) =
    (S.member (p1 + p2) pNumbers10kSet) && (S.member (abs(p1 - p2)) pNumbers10kSet)

cartesianProductOfPNumbers = [(p1, p2)|p1<- pNumbers10k, p2<- pNumbers10k, p1 < p2]

bothPNumber = take 20 $ filter isBothSumAndDiffArePentagonal cartesianProductOfPNumbers
