import Data.Bits
import Data.List.Split
import Data.List
import System.IO
import Data.Char
import Control.Applicative

normalChars = ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ [',','.','?','!','\n',' ','\''];
isNormalText text = (length $ filter (==False) $ map (flip elem normalChars) text) < 20

lowerChars = ['a'..'z']
secretKeys3 = [[a,b,c]| a<-lowerChars, b<-lowerChars, c<-lowerChars] 

decode encrypted keys =
  decode' (map ord keys) 0 (length keys) [] encrypted

decode' keys keyIndex keyLength decoded encrypted =
  if (encrypted == []) then (map chr keys, reverse decoded) 
  else let decyphered = (chr.xor (keys!!(keyIndex `mod` keyLength))) (head encrypted)
       in decode' keys (keyIndex + 1) keyLength (decyphered:decoded) (tail encrypted) 

main = do
  encrypted <- readFile "p059_cipher.txt"
  let secretMessage = map read $ splitOn "," encrypted
  let decodingResult = find (\(k,t) -> isNormalText t) (map (decode secretMessage) secretKeys3) 
  print $ sum <$> map ord <$> snd <$> decodingResult
