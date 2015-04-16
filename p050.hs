import Data.List

main = print $ foldl1 (\acc e -> if (fst acc > fst e) then acc else e) $ map maxPrimeSum [0..20]

isPrime::Int -> Bool
isPrime n = (n == 2) || (odd n && length [d|d<-[3,5..(floor.sqrt$fromIntegral n)], n `mod` d == 0] == 0)

maxPrimeSum dropCount =
    last $ takeWhile (\e -> snd e < 1000000) $ filter (\e -> isPrime (snd e)) $ scanl1 (\acc e -> (fst e, snd acc + snd e)) (zip [1..] (drop dropCount primes))

-- Below prime generator is copied from http://ideone.com/vkXCXt
primes :: [Int]
primes = [2,3,5,7] ++ _Y ((11:) . tail        -- no leak on 7.8.3 either
                   . gaps 11 wh11 
                   . _U . map (\(w,p)->scanl (\c x-> c+p*x) (p*p) w)
                   . hits 11 wh11)
  where
    gaps k (d:w) s@(c:t) | k < c     = k       : gaps (k+d) w s
                         | otherwise =           gaps (k+d) w t   -- k==c
    hits k (d:w) s@(p:t) | k < p     =           hits (k+d) w s
                         | otherwise = (d:w,p) : hits (k+d) w t   -- k==p
 
_Y g = g (_Y g)        -- multistage with non-sharing fixpoint combinator
     -- = g (fix g)    -- two stages with sharing fixpoint combinator
 
_U ((x:xs):t)   = x : (unionP xs . _U . pairs) t 
  where
        pairs (xs:ys:t) = unionP xs ys : pairs t
 
unionP a@(x:xs) b@(y:ys) = case compare x y of 
                                 LT -> x : unionP  xs b   
                                 EQ -> x : unionP  xs ys  
                                 GT -> y : unionP  a  ys  
 
wh3  = 2:wh3                -- n=1, d=2  [3, 5..] \\ ((3*) <$> _) 
wh5  = 2:4:wh5              -- n=2, d=6   [5,7,_, 11,..] \\ ((5*) <$> _)  
wh7  = 4:2:4:2:4:6:2:6:wh7  -- n=8,  d=30   [7,11,13,17,19,23,_,29,31,_, 37..]
wh11 =                      -- n=48, d=210                \\ ((7*) <$> _)
       2:4:2:4:6:2:6:4:2:4:6:6:2:6:4:2:6:4:6:8:4:2:4:2:  
       4:8:6:4:6:2:4:6:2:6:6:4:2:4:6:2:6:4:2:4:2:10:2:10:wh11  
 
