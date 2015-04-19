import Data.List hiding (union)

rmDuplicates :: (Ord a) => [a] -> [a]
rmDuplicates = map head . group . sort

isOddComposite::Int -> Bool
isOddComposite n = (odd n) && (length [d|d<-[3,5..(floor.sqrt$fromIntegral n)], n `mod` d == 0] > 0)
oddComposites = filter isOddComposite [9..]

primes2k = take 2000 primes
squares = map (^2) [1..100]

goldbach = rmDuplicates $ filter isOddComposite [p + 2*s| p <- primes2k, s <- squares] 

missingOddComposite (x:xs) (y:ys) =
    if (x /= y) then (x, y)
    else missingOddComposite xs ys

main = print $ fst $ missingOddComposite oddComposites goldbach

-- below code is copied from http://ideone.com/rHJ9ub
primes :: [Int]
primes = [2,3,5,7] ++ _Y ((11:) . tail . minus (scanl (+) 11 wh11) 
               . foldi (\(x:xs) r -> x : union xs r)
               . map (\(w,p)-> scanl (\c d-> c + p*d) (p*p) w)
               . equalsBy snd (tails wh11 `zip` scanl (+) 11 wh11))
 
wh3  = 2:wh3                 --  ([3],2)                {1*2,2*3}
wh5  = 2:4:wh5               --  ([5,7],6)                  {2*4,6*5}
wh7  = 4:2:4:2:4:6:2:6:wh7   --  ([7,11,13,17,19,23,29,31],30)  {8*6,30*7}
wh11 = 2:4:2:4:6:2:6:4:2:4:6:6:2:6:4:2:6:4:6:8:4:2:4:2:  
       4:8:6:4:6:2:4:6:2:6:6:4:2:4:6:2:6:4:2:4:2:10:2:10:wh11
 
_Y g = g (_Y g)        -- multistage with non-sharing fixpoint combinator
     -- = g (fix g)    -- two stages with sharing fixpoint combinator
 
foldi f (h:t) = f h . foldi f . unfoldr (\(a:b:c)->Just(f a b,c)) $ t
 
union      a b = ordzipBy id  (:)  (:)  (:)  a b
minus      a b = ordzipBy id  (:) skip skip  a b
equalsBy k a b = ordzipBy k   skip (:) skip  a b
skip       a b = b                       -- skip a = [] ; emit a = [a]
 
ordzipBy k f g h a b = loop a b where    -- concat$unfoldr pull(a,b)
  loop a@(x:t) b@(y:r) = case compare (k x) y of
    LT -> f x (loop t b)                 -- Just(f x,(t,b))
    EQ -> g x (loop t r)                 -- Just(g x,(t,r))
    GT -> h y (loop a r)                 -- Just(h y,(a,r))
