

import qualified Data.Map as Map

defs :: Map.Map Int String
defs = Map.fromList [
  (1, "I")
 ,(4, "IV")
 ,(5, "V")
 ,(9, "IX")
 ,(10, "X")
 ,(40, "XL")
 ,(50, "L")
 ,(90, "XC")
 ,(100, "C")
 ,(400, "CD")
 ,(500, "D")
 ,(900, "CM")
 ,(1000, "M")]

procPart :: Int -> Int -> String
procPart n b = case Map.lookUp n defs of
                 Nothing ->
								 Just s -> s

import qualified Data.ByteString.Lazy.Char8 as L
import qualified Data.ByteString.Char8 as C

main  = do
          content <- L.getContents 
          let linesOfFile = L.lines content
          let l1 = [(L.split ',') lineOfFile | lineOfFile <- linesOfFile]
          let l2 = [process lineOfFile | lineOfFile <- l1]
          let outputLines = [putStr y | y <- l2]
          seqn outputLines

seqn :: [IO a] -> IO ()
seqn [] = return ()
seqn (x : xs) = do
                  x; putChar '\n'
                  seqn xs

process :: [L.ByteString] -> [Char]
process [] = "None"
process lst = if (countElem lst maj) > hlf then (show maj) else "None"
	            where
                (maj,tot) = findMajCandidate lst 0 0 0
                hlf = tot `div` 2

countElem :: [L.ByteString] -> Int -> Int
countElem lst n = foldr (\i v -> fnd i v n) 0 lst

fnd :: L.ByteString -> Int -> Int -> Int
fnd i v n = if t == n then (v + 1) else v
              where
						   t = case L.readInt i of
								  	 Just (a,_) -> a

findMajCandidate :: [L.ByteString] -> Int -> Int -> Int -> (Int,Int)
findMajCandidate [] _ m cnt = (m,cnt)
findMajCandidate (s:ss) c m cnt 
  | num == m = findMajCandidate ss (c + 1) m (cnt + 1)
	| c == 0 = findMajCandidate ss 1 num (cnt + 1)
  | otherwise = findMajCandidate ss (c - 1) m (cnt + 1)
  where
    num = case L.readInt s of
            Nothing -> error "hi"
            Just (j,_) -> j
