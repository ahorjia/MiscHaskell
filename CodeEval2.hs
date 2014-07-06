
import qualified Data.Map as Map

queryBoard :: Map.Map (Int,Int) Int
queryBoard = Map.fromList [((i,j),0) | i <- [0..boardUB], j <-[0..boardUB]]

boardUB :: Int
boardUB = 5 

queryRow :: Map.Map (Int,Int) Int -> Int -> Int
queryRow mp row = sum $ selectRocCol mp [(row,j) | j <- [0..boardUB]]

queryCol :: Map.Map (Int,Int) Int -> Int -> Int
queryCol mp col = sum $ selectRocCol mp [(i,col) | i <- [0..boardUB]]

selectRocCol :: Map.Map (Int,Int) Int -> [(Int,Int)] -> [Int]
selectRocCol _ [] = []
selectRocCol mp ((i,j):rest) = (mp Map.! (i,j)):(selectRocCol mp rest)

setRow :: Map.Map (Int,Int) Int -> Int -> Int -> Map.Map (Int,Int) Int
setRow mp row val = setRowCol mp [(row,j) | j <- [0..boardUB]] val

setCol :: Map.Map (Int,Int) Int -> Int -> Int -> Map.Map (Int,Int) Int
setCol mp col val = setRowCol mp [(i,col) | i <- [0..boardUB]] val

setRowCol :: Map.Map (Int,Int) Int -> [(Int,Int)] -> Int -> Map.Map (Int,Int) Int
setRowCol mp [] _ = mp
setRowCol mp ((i,j):rest) val = setRowCol (Map.adjust (\_ -> val) (i,j) mp) rest val

{-
import Data.Array
import qualified Data.Map as Map
import Data.List (insert)

main  = do
 content <- getContents
 let linesOfFile = lines content
 let l1 = [process $ map (\x -> read x::Int) (split lineOfFile ',') | lineOfFile <- linesOfFile]
 let outputLines = [putStr $ show y | y <- l1]
 seqn outputLines

seqn :: [IO a] -> IO ()
seqn [] = return ()
seqn (x : xs) = do
  x; putChar '\n'
  seqn xs

split :: String -> Char -> [String]
split [] delim = [""]
split (x:xs) delim
	| x == delim = "" : rest
	| otherwise = (x : head rest) : tail rest
	where
		rest = split xs delim

process :: [Int] -> Int
process lst = length $ foldRep $ insertionSort $ innerInsertionSort $ findAll sp mp
              where
                sp = sumPair lst
                mp = buildMp Map.empty sp

findAll :: [((Int,Int),Int)] -> Map.Map Int [(Int,Int)] -> [(Int,Int,Int,Int)]
findAll [] _ = []
findAll (((i1,j1),n):rest) mp = if (Map.member (-n) mp) && (-n) >= n then [(i1,j1,i2,j2) | (i2,j2) <- ps, i1 /= i2 && i1 /= j2 && j1 /= i2 && j1 /= j2] ++ findAll rest mp
                                else findAll rest mp
                                  where
                                    ps = mp Map.! (-n)

sumPair :: [Int] -> [((Int,Int),Int)]
sumPair lst = [((x,y),x+y) | x <- lst, y <- lst, x < y]

buildMp :: Map.Map Int [(Int,Int)] -> [((Int,Int),Int)] -> Map.Map Int [(Int,Int)]
buildMp mp [] = mp
buildMp mp ((p,n):rest) = buildMp (Map.insertWith (++) n [p] mp) rest

foldRep :: (Ord a) => [(a,a,a,a)] -> [(a,a,a,a)]
foldRep [] = []
foldRep [x] = [x]
foldRep (x:y:rest) = if x == y then foldRep (y:rest) else x : foldRep (y:rest)

insertionSort :: (Ord a) => [(a,a,a,a)] -> [(a,a,a,a)]
insertionSort = foldr insert []

innerInsertionSort [] = []
innerInsertionSort ((a,b,c,d):rest) = (x,y,z,t) : innerInsertionSort rest
                                        where
                                          (x:y:z:t:_) = foldr insert [] [a,b,c,d]

insertionSort :: (Ord a, Ord b) => [(a,b)] -> [(a,b)]
insertionSort = foldr insert []

import Data.List

main  = do
 content <- getContents
 let linesOfFile = lines content
 let l1 = [process lineOfFile | lineOfFile <- linesOfFile]
 let outputLines = [putStr $ foldResult y | y <- l1]
 seqn outputLines

seqn :: [IO a] -> IO ()
seqn [] = return ()
seqn (x : xs) = do
  x; putChar '\n'
  seqn xs

process :: [Char] -> [(Int,String)]
process [] = []
process str = qsort $ zip o1 ws
              where
                sp = split str ';'
                ws = words $ head sp
                od = map (\x -> read x::Int) (words $ head $ tail sp)
                o1 = od ++ [findMis od $ (length od) + 1]

split :: String -> Char -> [String]
split [] delim = [""]
split (x:xs) delim
	| x == delim = "" : rest
	| otherwise = (x : head rest) : tail rest
	where
		rest = split xs delim

foldResult :: [(Int,String)] -> [Char]
foldResult [] = ""
foldResult [(_,s)] = s
foldResult ((_,s):xs) = s ++ " " ++ foldResult xs

findMis :: [Int] -> Int -> Int
findMis lst cnt = (cnt * (cnt + 1)) `div` 2 - sum lst 

qsort [] = []
qsort (p@(x,_):xs) = qsort ys ++ p : qsort zs
				where
					(ys, zs) = partition (\(n,_) ->  n < x) xs

qs [] = []
qs (x:xs) = qs ys ++ x : qs zs
				where
					(ys, zs) = partition ( < x) xs
main  = do
 content <- getContents
 let linesOfFile = lines content
 let l1 = [map (\x -> read x::Int) (words lineOfFile) | lineOfFile <- linesOfFile]
 let l2 = [curInit lineOfFile | lineOfFile <- l1]
 let outputLines = [putStr $ foldResult y | y <- l2]
 seqn outputLines

foldResult :: [Int] -> [Char]
foldResult [] = ""
foldResult [x] = (show x)
foldResult (x:xs) = (show x) ++ " " ++ foldResult xs

seqn :: [IO a] -> IO ()
seqn [] = return ()
seqn (x : xs) = do
  x; putChar '\n'
  seqn xs

curInit :: [Int] -> [Int]
curInit [] = []
curInit (n:ns) = curCnt ns n 1

curCnt :: [Int] -> Int -> Int -> [Int]
curCnt [] n c = [c,n]
curCnt (x:xs) n c = if n == x then curCnt xs n (c+1)
                              else c : n : curCnt xs x 1



import qualified Data.Map as Map

main  = do
 content <- getContents
 let linesOfFile = lines content
 let l1 = [map (\x -> read x::Int) (split lineOfFile ',')| lineOfFile <- linesOfFile]
 let l2 = [findMaj (Map.toList (buildMap initMap lineOfFile)) (length lineOfFile)| lineOfFile <- l1]
 let outputLines = [putStr $ readMaybe y | y <- l2]
 seqn outputLines

readMaybe m = case m of
		Just n -> show n
                Nothing -> "None"

seqn :: [IO a] -> IO ()
seqn [] = return ()
seqn (x : xs) = do
  x; putChar '\n'
  seqn xs

split :: String -> Char -> [String]
split [] delim = [""]
split (x:xs) delim
	| x == delim = "" : rest
	| otherwise = (x : head rest) : tail rest
	where
		rest = split xs delim

findMaj :: [(Int,Int)] -> Int -> Maybe Int
findMaj [] _ = Nothing
findMaj ((n,c):rest) l = if c >= ((l `div` 2) + 1) then Just n else findMaj rest l

initMap = Map.fromList [(x,0) | x <- [0..100]]

buildMap :: Map.Map Int Int -> [Int] -> Map.Map Int Int
buildMap mp [] = mp
buildMap mp (n:ns) = buildMap (Map.adjust (1 +) n mp) ns

import Data.List

main  = do
 content <- getContents
 let linesOfFile = lines content
 let l1 = [zip (map (\x -> read x::Int) (words lineOfFile)) [1..] | lineOfFile <- linesOfFile]
 let l2 = [trio $ qsort lineOfFile | lineOfFile <- l1]
 let outputLines = [putStr $ show y | y <- l2]
 seqn outputLines

seqn :: [IO a] -> IO ()
seqn [] = return ()
seqn (x : xs) = do
  x; putChar '\n'
  seqn xs

trio :: [(Int,Int)] -> Int
trio [] = 0 
trio [(a,ar)] = ar
trio [all@(a,ar),(b,_)] = if a /= b then ar else 0
trio (all@(a,ar):bll@(b,_):cll@(c,_):res) = if a == b && b == c then trio (bll:cll:res) else
			if a == b then trio (cll:res) else
			ar

qsort [] = []
qsort ((x,r):xs) = qsort [is | is@(i,_) <- xs, i <= x] ++ [(x,r)] ++ qsort [is | is@(i,_) <- xs, i > x]

import qualified Data.Map as Map
import Data.List (insert)

test1 = [('6','D'), ('7','H'), ('A','H'), ('7','S'), ('Q','C')]
test2 = [('6','H') ,('2','D') ,('T','D') ,('J','D'), ('A','S')]
test3 = [('T','H') ,('J','H') ,('Q','H') ,('K','H'), ('A','H')]
test4 = [('T','H') ,('J','H') ,('Q','H') ,('8','H'), ('9','H')]
test5 = [('T','H') ,('T','H') ,('Q','H') ,('Q','H'), ('Q','H')]
test6 = [('4','H') ,('2','H') ,('3','H') ,('Q','H'), ('Q','H')]
test7 = [('5','H') ,('6','H') ,('7','Q') ,('8','H'), ('9','H')]

comp left right =
		case compare leftS rightS of
			LT -> "right"
			GT -> "left"
			EQ -> "none"
			where
				(leftS, leftX) = score left
				(rightS, rightX) = score right

score lst | isRoyalFlush nLst = (10, [])
		  | isStraightFlush nLst = (9, [head nLst])
		  | isFourOfAKind mpLst = (8, [])
		  | isFullHouse mpLst = (7, [])
		  | isFlush nLst = (6, reverse nLst)
		  | isStraight nLst = (5, [head nLst])
		  | isThreeOfAKind mpLst = (4, [])
		  | isTwoPairs mpLst = (3, [])
		  | isOnePair mpLst = (2, [])
		  | otherwise = (1, reverse nLst)
			where
				nLst = insertionSort $ procNs lst
				cntMp = sameCount nLst initMap 
				mpLst = Map.toList cntMp
			
isOnePair lst = tw == 1
					where
						((tw,tws),_,_) = countKinds lst ((0,[]),(0,[]),(0,[]))

isTwoPairs lst = tw == 2
					where
						((tw,tws),_,_) = countKinds lst ((0,[]),(0,[]),(0,[]))
						
isThreeOfAKind lst = th == 1
						where
							(_,(th,ths),_) = countKinds lst ((0,[]),(0,[]),(0,[]))
							
isFullHouse lst = th == 1 && tw == 1
					where
						((tw,tws),(th,ths),_) = countKinds lst ((0,[]),(0,[]),(0,[]))
						
isFourOfAKind lst = f > 0
					where
						(_,_,(f,fs)) = countKinds lst ((0,[]),(0,[]),(0,[]))

countKinds [] cnts = cnts
countKinds ((n,c):rest) res@(w@(tw,tws),h@(th,ths),f1@(f,fs))
		| c == 4 = countKinds rest (w, h, ((1 + f),(n,c):fs))
		| c == 3 = countKinds rest (w, (1 + th,(n,c):ths), f1)
		| c == 2 = countKinds rest ((1 + tw,(n,c):tws), h, f1)
		| otherwise = countKinds rest res

isStraight lst = conseq lst

isFlush lst = sameSuit lst

isStraightFlush lst = sameSuit lst && conseq lst

isRoyalFlush lst = sameSuit lst && conseq lst && (fst (head lst) == 10)

convMap :: Map.Map Char Int
convMap = Map.fromList $ (zip ['2'..'9'] [2..9]) ++ [('T',10),('J',11),('Q',12),('K',13),('A',14)]

procNs :: [(Char,Char)] -> [(Int,Char)]
procNs [] = []
procNs ((nc,s):rest) = (convMap Map.! nc,s) : procNs rest

initMap :: Map.Map Int Int
initMap = Map.fromList [(x,0) | x <- [2..14]]

sameCount :: [(Int,Char)] -> Map.Map Int Int -> Map.Map Int Int
sameCount [] mp = mp
sameCount ((n,_):rest) mp = sameCount rest (Map.adjust (1 +) n mp)

procHand :: [(Int,Char)] -> ((Int,Char) -> (Int,Char) -> Bool) -> Bool
procHand [] _ = True
procHand [_] _ = True
procHand (p1:p2:rest) f = if not $ f p1 p2 then False else procHand (p2:rest) f

sameSuit :: [(Int,Char)] -> Bool
sameSuit lst = procHand lst (\(_,s1) (_,s2) -> s1 == s2)

conseq :: [(Int,Char)] -> Bool
conseq lst = procHand lst (\(n1,_) (n2,_) -> n2 == (n1 + 1))

insertionSort :: (Ord a, Ord b) => [(a,b)] -> [(a,b)]
insertionSort = foldr insert []

import System.Environment( getArgs )
import Data.Sequence
import Prelude hiding (replicate)

-- Disjoint set data type (weighted and using path compression).
-- O((M+N)lg*N + 2MlogN) worst-case union time (practically O(1))
-- For M union operations on a set of N elements.
-- O((M+N)lg*N) worst-case find time (practically O(1))
-- For M connected(find) operations on a set of N elements.
data DisjointSet = DisjointSet
     { count :: Int, ids :: (Seq Int), sizes :: (Seq Int) }
     deriving (Read,  Show)

-- Return id of root object
findRoot :: DisjointSet -> Int -> Int
findRoot set p | p == parent = p
               | otherwise   = findRoot set parent
               where
                parent = index (ids set) (p - 1)

-- Are objects P and Q connected ?
connected :: DisjointSet -> Int -> Int -> Bool
connected set p q = (findRoot set p) == (findRoot set q)

-- Replace sets containing P and Q with their union
quickUnion :: DisjointSet -> Int -> Int -> DisjointSet
quickUnion set p q | i == j = set
                   | otherwise = DisjointSet cnt rids rsizes
                     where
                        (i, j)   = (findRoot set p, findRoot set q)
                        (i1, j1) = (index (sizes set) (i - 1), index (sizes set) (j - 1))
                        (cnt, psmaller, size) = (count set - 1, i1 < j1, i1 + j1)
                        -- Always make smaller root point to the larger one
                        (rids, rsizes) = if psmaller
                                         then (update (i - 1) j (ids set), update (j - 1) size (sizes set))
                                         else (update (j - 1) i (ids set), update (i - 1) size (sizes set))

import Data.Char  (digitToInt)
import Data.Maybe (listToMaybe)
import Numeric    (readInt)

main  = do
			content <- getContents
			let linesOfFile = lines content
			let l1 = [readBinVal $ readBin $ conv (words lineOfFile) | lineOfFile <- linesOfFile]
			let outputLines = [putStr $ show y | y <- l1]
			seqn outputLines

seqn :: [IO a] -> IO ()
seqn [] = return ()
seqn (x : xs) = do
					x; putChar '\n'
					seqn xs

readBinVal val = case val of
					Just x -> x
					Nothing -> 0
					
readBin :: Integral a => String -> Maybe a
readBin = fmap fst . listToMaybe . readInt 2 (`elem` "01") digitToInt

conv :: [[Char]] -> [Char]
conv [] = []
conv (flag:digs:rest) = (convReq flag digs) ++ conv rest

convReq :: [Char] -> [Char] -> [Char]
convReq flag digs | flag == "0" = ['0' | _ <- digs]
				| flag == "00" = ['1' | _ <- digs]
				
import Data.Array

qsort [] = []
qsort (x:xs) = qsort ys ++ x : qsort zs
				where
					(ys, zs) = partition ( < x) xs

pairSum :: [Int] -> [(Int,Int,Int)]
pairSum [x,y] = [(x+y,x,y)]
pairSum (x:xs) = (map (\t -> (t + x, x, t)) xs) ++ pairSum xs

binarySearchArray :: (Ix i, Integral i, Ord e) => Array i e -> e -> Maybe i
binarySearchArray a x = binarySearch p (bounds a)
						where
							p m = x `compare` (a ! m)

binarySearch :: Integral a => (a -> Ordering) -> (a, a) -> Maybe a
binarySearch p (low,high) 
  | high < low = Nothing
  | otherwise = 
      let mid = (low + high) `div` 2 in 
      case p mid of
        LT -> binarySearch p (low, mid-1)
        GT -> binarySearch p (mid+1, high)
        EQ -> Just mid

import Data.Fixed

main  = do
			content <- getContents
			let linesOfFile = lines content
			let l1 = [par lineOfFile | lineOfFile <- linesOfFile]
			let l2 = ["RGB(" ++ (show r) ++ "," ++ (show g) ++ "," ++ (show b) ++ ")" | (r,g,b) <- l1]
			let outputLines = [putStr y | y <- l2]
			seqn outputLines

seqn :: [IO a] -> IO ()
seqn [] = return ()
seqn (x : xs) = do
					x; putChar '\n'
					seqn xs

par :: [Char] -> (Int,Int,Int)
par all@('(':cs) = cmyk2rgb $ breakNum4 all
par ('H':'S':'L':cs) = hsl2rgb $ breakNum3 cs
par ('H':'S':'V':cs) = hsv2rgb $ breakNum3 cs
par ('#':cs) = hex2rgb (r1,g1,b1)
				where
					(c1:c2:c3:c4:c5:c6:_) = cs
					(r1,g1,b1) = ([c1,c2],[c3,c4],[c5,c6])
					
breakNum3 :: [Char] -> (Double,Double,Double)
breakNum3 lst = (d1,d2,d3)
				where
					(d1:d2:d3:_) = map (\x -> read x::Double) (split (init (tail lst)) ',')

breakNum4 :: [Char] -> (Double,Double,Double,Double)
breakNum4 lst = (d1,d2,d3,d4)
				where
					(d1:d2:d3:d4:_) = map (\x -> read x::Double) (split (init (tail lst)) ',')

split :: String -> Char -> [String]
split [] delim = [""]
split (x:xs) delim
	| x == delim = "" : rest
	| otherwise = (x : head rest) : tail rest
	where
		rest = split xs delim

hex2rgb :: (String,String,String) -> (Int,Int,Int)
hex2rgb (r1,g1,b1) = (hex2rgbUnit r1,hex2rgbUnit g1,hex2rgbUnit b1)

hex2rgbUnit r1 = read ("0x" ++ r1)::Int

hsv2rgb :: (Double,Double,Double) -> (Int,Int,Int)
hsv2rgb (h,s',v') = (round $ r1 * 255, round $ g1 * 255, round $ b1 * 255)
					where
						s = s' * 0.01
						v = v' * 0.01
						h' = h / 60
						i = fromIntegral $ floor h'
						f = h' - i
						p = v * (1 - s)
						q = v * (1 - s * f)
						t = v * (1 - s * (1 - f))
						(r1,g1,b1) = case i of 
										0 -> (v,t,p)
										1 -> (q,v,p)
										2 -> (p,v,t)
										3 -> (p,q,v)
										4 -> (t,p,v)
										otherwise -> (v,p,q)

hsl2rgb :: (Double,Double,Double) -> (Int,Int,Int)
hsl2rgb (h,s',l') = (round $ 255 * (r1 + m), round $ 255 * (g1 + m), round $ 255 * (b1 + m))
					where
						s = s' * 0.01
						l = l' * 0.01
						c = (1 - abs (2 * l - 1)) * s
						h' = h / 60
						x = c * (1 - abs (h' `mod'` 2 - 1))
						(r1,g1,b1) 	| 0 <= h' && h' < 1 = (c,x,0)
									| 1 <= h' && h' < 2 = (x,c,0)
									| 2 <= h' && h' < 3 = (0,c,x)
									| 3 <= h' && h' < 4 = (0,x,c)
									| 4 <= h' && h' < 5 = (x,0,c)
									| 5 <= h' && h' < 6 = (c,0,x)
									| otherwise = (0,0,0)
						m = l - c / 2

cmyk2rgb :: (Double,Double,Double,Double) -> (Int,Int,Int)
cmyk2rgb (c,m,y,k) = (round $ 255 * (1 - c) * (1 - k), round $ 255 * (1 - m) * (1 - k), round $ 255 * (1 - y) * (1 - k))
{
import Data.Array

lst :: Int -> [Int]
lst n = map (^2) [0..(round $ sqrt $ fromIntegral n)]

sqs :: [Int] -> Array Int Int
sqs lst = listArray (0,(length lst) - 1) lst 

binarySearchArray :: (Ix i, Integral i, Ord e) => Array i e -> e -> Maybe i
binarySearchArray a x = binarySearch p (bounds a)
						where
							p m = x `compare` (a ! m)

binarySearch :: Integral a => (a -> Ordering) -> (a, a) -> Maybe a
binarySearch p (low,high) 
  | high < low = Nothing
  | otherwise = 
      let mid = (low + high) `div` 2 in 
      case p mid of
        LT -> binarySearch p (low, mid-1)
        GT -> binarySearch p (mid+1, high)
        EQ -> Just mid

main  = do
			content <- getContents
			let linesOfFile = lines content
			let l1 = [split lineOfFile ';'| lineOfFile <- linesOfFile]
			let l2 = [(read (head lineOfFile)::Int, map (\x -> read x::Int) (split (head (tail lineOfFile)) ',')) | lineOfFile <- l1]
			let outputLines = [putStr (show (mis n lst)) | (n,lst) <- l2]
			seqn outputLines

seqn :: [IO a] -> IO ()
seqn [] = return ()
seqn (x : xs) = do
					x; putChar '\n'
					seqn xs

split :: String -> Char -> [String]
split [] delim = [""]
split (x:xs) delim
	| x == delim = "" : rest
	| otherwise = (x : head rest) : tail rest
	where
		rest = split xs delim

mis :: Int -> [Int] -> Int
mis n lst = n - 1 + sum lst - (n * (n - 1)) `div` 2
-}
{-
import Data.Array

main  = do
			content <- getContents
			let linesOfFile = lines content
			let l1 = [rep lineOfFile | lineOfFile <- linesOfFile]
			let outputLines = [putStr lineOfFile | lineOfFile <- l1]
			seqn outputLines

seqn :: [IO a] -> IO ()
seqn [] = return ()
seqn (x : xs) = do
					x; putChar '\n'
					seqn xs

rep :: [Char] -> [Char]
rep [] = []
rep (c:cs)  | c == ' ' = ' ' : rep cs
			| otherwise = arr ! c : rep cs
			
arr :: Array Char Char
arr = listArray ('a','z') ['y','h','e','s','o','c','v','x','d','u','i','g','l','b','k','r','z','t','n','w','j','p','f','m','a','q'] 
-}
{-
import Data.List
 
qsort [] = []
qsort (x:xs) = qsort ys ++ x : qsort zs
				where
					(ys, zs) = partition ( < x) xs

binarySearchArray :: (Ix i, Integral i, Ord e) => Array i e -> e -> Maybe i
binarySearchArray a x = binarySearch p (bounds a)
						where
							p m = x `compare` (a ! m)

binarySearch :: Integral a => (a -> Ordering) -> (a, a) -> Maybe a
binarySearch p (low,high) 
  | high < low = Nothing
  | otherwise = 
      let mid = (low + high) `div` 2 in 
      case p mid of
        LT -> binarySearch p (low, mid-1)
        GT -> binarySearch p (mid+1, high)
        EQ -> Just mid

-}
{-
main  = do
			content <- getContents
			let linesOfFile = lines content
			let l1 = [map (\x -> read x::Int) (split lineOfFile ',') | lineOfFile <- linesOfFile]
			let l2 = [filt [0..((head lineOfFile) - 1)] [] (head (tail lineOfFile)) 1 | lineOfFile <- l1]
			let outputLines = [putStr (foldResult lineOfFile) | lineOfFile <- l2]
			seqn outputLines

foldResult :: [Int] -> [Char]
foldResult [] = ""
foldResult [x] = (show x)
foldResult (x:xs) = (show x) ++ " " ++ foldResult xs

seqn :: [IO a] -> IO ()
seqn [] = return ()
seqn (x : xs) = do
					x; putChar '\n'
					seqn xs

split :: String -> Char -> [String]
split [] delim = [""]
split (x:xs) delim
	| x == delim = "" : rest
	| otherwise = (x : head rest) : tail rest
	where
		rest = split xs delim

filt :: [Int] -> [Int] -> Int -> Int -> [Int]
filt [] [] _ _ = []
filt [] nq n c = filt (reverse nq) [] n c
filt (dq:dqs) eq n c = if c == n then (dq : (filt dqs eq n 1)) else (filt dqs (dq:eq) n (c+1))

import Text.Printf

main  = do
			content <- getContents
			let linesOfFile = lines content
			let l1 = [printBoth lineOfFile | lineOfFile <- linesOfFile]
			let l2 = ["lowercase: " ++ (fst lineOfFile) ++ " uppercase: " ++ (snd lineOfFile) | lineOfFile <- l1]
			let outputLines = [putStr lineOfFile | lineOfFile <- l2]
			seqn outputLines

seqn :: [IO a] -> IO ()
seqn [] = return ()
seqn (x : xs) = do
					x; putChar '\n'
					seqn xs

printBoth :: [Char] -> (String,String)
printBoth str = (printf "%.2f" a, printf "%.2f" b)
			where
				(a,b) = perBoth str
				
perBoth :: [Char] -> (Double,Double)
perBoth str = (val, 100.00 - val)
				where
					val = perLower str
					
perLower :: [Char] -> Double
perLower str = ((fromIntegral (cntLower str)) / (fromIntegral (length str))) * 100.00

cntLower :: [Char] -> Int
cntLower [] = 0
cntLower (c:cs) = if aE <= cE && cE <= zE then 1 + cntLower cs else cntLower cs
					where
						cE = fromEnum c
						aE = fromEnum 'a'
						zE = fromEnum 'z'

import qualified Data.Map as Map

main  = do
			content <- getContents
			let linesOfFile = lines content
			let l1 = [dec lineOfFile| lineOfFile <- linesOfFile]
			let l2 = [if lineOfFile == "" then "NONE" else lineOfFile | lineOfFile <- l1]
			let outputLines = [putStr lineOfFile | lineOfFile <- l2]
			seqn outputLines

seqn :: [IO a] -> IO ()
seqn [] = return ()
seqn (x : xs) = do
					x; putChar '\n'
					seqn xs

win str = if w == (fromEnum '0') then 0 else w - fromEnum '0' - 1
			where
				mp = updt cnt str
				w = fromEnum (winner mp ['0'..'9'])

winner :: Map.Map Char Int -> [Char] -> Char
winner mp [] = '0'
winner mp (c:cs) = if mp Map.! c == 1 then c else winner mp cs

cnt :: Map.Map Char Int
cnt = Map.fromList [(x,0) | x <- ['0'..'9']]

updt :: Map.Map Char Int -> [Char] -> Map.Map Char Int
updt mp [] = mp
updt mp (c:cs) = updt (Map.adjust (1 +) c mp) cs


import Text.ParserCombinators.Parsec

csvFile = endBy line eol

line = sepBy cell (char ',')

cell = many (noneOf ",\n")

eol = char '\n'

parseCSV :: String -> Either ParseError [[String]]

seqn :: [IO a] -> IO ()
seqn [] = return ()
seqn (x : xs) = do
					x; putChar '\n'
					seqn xs

split :: String -> Char -> [String]
split [] delim = [""]
split (x:xs) delim
	| x == delim = "" : rest
	| otherwise = (x : head rest) : tail rest
	where
		rest = split xs delim

process :: String -> Integer
process str = if splitPlus1 /= str then splitPlus1Num + splitPlus2Num else splitMinus1Num - splitMinus2Num
				where
					splitPlus = split str '+'
					splitMinus = split str '-'
					splitPlus1 = head splitPlus
					splitPlus2 = head (tail splitPlus)
					splitMinus1 = head splitMinus
					splitMinus2 = head (tail splitMinus)
					splitMinus1Num = read splitMinus1::Integer
					splitMinus2Num = read splitMinus2::Integer
					splitPlus1Num = read splitPlus1::Integer
					splitPlus2Num = read splitPlus2::Integer

import Data.Array

main  = do
			content <- getContents
			let linesOfFile = lines content
			let l1 = [dec lineOfFile| lineOfFile <- linesOfFile]
			let l2 = [if lineOfFile == "" then "NONE" else lineOfFile | lineOfFile <- l1]
			let outputLines = [putStr lineOfFile | lineOfFile <- l2]
			seqn outputLines

seqn :: [IO a] -> IO ()
seqn [] = return ()
seqn (x : xs) = do
					x; putChar '\n'
					seqn xs

arr :: Array Char Char
arr = listArray ('a','j') ['0'..'9']

dec :: [Char] -> [Char]
dec [] = []
dec (c:cs)  | cE >= aE && cE <= jE = (arr ! c) : dec cs
			| cE >= zE && cE <= nE = c : dec cs
			| otherwise = dec cs
				where
					cE = fromEnum c
					aE = fromEnum 'a'
					jE = fromEnum 'j'
					zE = fromEnum '0'
					nE = fromEnum '9'

-- file: ch16/csv2.hs
parseCSV input = parse csvFile "(unknown)" input
-}

{-
import Data.Array

major ns| (ds ! 1) == (ds ! ((l `div` 2) + 1)) = show (ds ! 1)
		| (ds ! (length ns)) == (ds ! ((l `div` 2) - 1)) = show (ds ! (length ns))
		| otherwise = "None"
			where
				l = length ns
				ds = val ns
				n1 = ds ! 1
				n2 = ds ! l
				n3 = ds ! (l `div` 2)
				n4 = ds ! ((l `div` 2) + 1)
				n5 = ds ! ((l `div` 2) - 1)
				
val ns = listArray (1, length ns) (qsort ns)

qsort [] = []
qsort (c:cs) = qsort smaller ++ [c] ++ qsort larger
				where
					smaller = [x | x <- cs, x <= c]
					larger = [x | x <- cs, x > c]

main  = do
			content <- getContents
			let linesOfFile = lines content
			let l1 = [split lineOfFile ','| lineOfFile <- linesOfFile]
			let l2 = [separate lineOfFile | lineOfFile <- l1]
			let outputLines = [putStr (printOutput lineOfFile) | lineOfFile <- l2]
			seqn outputLines

printOutput :: ([String],[String]) -> String
printOutput ([],[]) = ""
printOutput (ns,[]) = foldResult ns
printOutput ([],ws) = foldResult ws
printOutput (ns,ws) = foldResult ws ++ "|" ++ foldResult ns

split :: String -> Char -> [String]
split [] delim = [""]
split (x:xs) delim
	| x == delim = "" : rest
	| otherwise = (x : head rest) : tail rest
	where
		rest = split xs delim

foldResult :: [[Char]] -> [Char]
foldResult [] = ""
foldResult [x] = x
foldResult (x:xs) = x ++ "," ++ foldResult xs

seqn :: [IO a] -> IO ()
seqn [] = return ()
seqn (x : xs) = do
					x; putChar '\n'
					seqn xs

separate :: [String] -> ([String],[String])
separate [] = ([],[])
separate (str:strs) = if isNum str then (str:ns,ws) else (ns,str:ws)
						where
							(ns,ws) = separate strs

isNum :: [Char] -> Bool
isNum [] = True
isNum (c:cs) = isDigit && isNum cs
			where
				cNum = fromEnum c
				zNum = fromEnum '0'
				nNum = fromEnum '9'
				isDigit = zNum <= cNum && cNum <= nNum

main  = do
			content <- getContents
			let linesOfFile = lines content
			let l1 = [processInput lineOfFile | lineOfFile <- linesOfFile]
			let l2 = [fb (fst $ fst lineOfFile) (snd $ fst lineOfFile) (snd lineOfFile) 1 | lineOfFile <- l1]
			let outputLines = [putStr (foldResult lineOfFile) | lineOfFile <- l2]
			seqn outputLines

processInput :: String -> ((Int,Int),Int)
processInput inp = ((head items, head $ tail items), head $ tail $ tail items)
					where
						items = map (\x -> read x::Int) (words inp)

seqn :: [IO a] -> IO ()
seqn [] = return ()
seqn (x : xs) = do
					x; putChar '\n'
					seqn xs

foldResult :: [[Char]] -> [Char]
foldResult [] = ""
foldResult [x] = x
foldResult (x:xs) = x ++ " " ++ foldResult xs

fb :: Int -> Int -> Int -> Int -> [String]
fb a b n c  | c > n = []
			| divAB = "FB" : (fb a b n (c + 1))
			| divB = "B" : (fb a b n (c + 1))
			| divA = "F" : (fb a b n (c + 1))
			| otherwise = (show c) : (fb a b n (c + 1))
			where 
				divA = c `mod` a == 0
				divB = c `mod` b == 0
				divAB = divA && divB

--import qualified Data.Sequence as DS
module Main( main ) where

import System.Environment( getArgs )
import Data.Sequence
import Prelude hiding (replicate)

tcnt :: Int
tcnt = 20

--DS.index (sizes (allUnionCoord set [(1,0),(1,1)])) 0
--buildUF = set
--			where
--				find19Nodes = [(x,y) | x <- [1..tcnt], y <- [0..tcnt], (sum (extractDigitsRevRecur x) + sum (extractDigitsRevRecur y)) <= 19]
--				size = length find19Nodes
--				set = DisjointSet size (DS.fromList [(x - 1) * (tcnt + 1) + y | (x,y) <- find19Nodes]) (DS.fromList [1 | _ <- [1..size]])  

allUnionCoord :: DisjointSet -> [(Int,Int)] -> DisjointSet
allUnionCoord set [] = set
allUnionCoord set (p:ps) = allUnionCoord (allUnionCoordRec set p) ps

allUnionCoordRec :: DisjointSet -> (Int,Int) -> DisjointSet
allUnionCoordRec set (1,0) = set
allUnionCoordRec set (1,j) = quickUnionCoord set (1,j) (1,j-1)
allUnionCoordRec set (i,0) = quickUnionCoord set (i,0) (i-1,0)
allUnionCoordRec set (i,j) = quickUnionCoord (quickUnionCoord set (i,j) (i-1,j)) (i,j) (i,j-1)

quickUnionCoord :: DisjointSet -> (Int,Int) -> (Int,Int) -> DisjointSet
quickUnionCoord set (i1,j1) (i2,j2) = if p2_19 && p1_19 then quickUnion set ind1 ind2 else set
										where
											ind1 = (i1 - 0) * (tcnt + 1) + j1
											ind2 = (i2 - 0) * (tcnt + 1) + j2
											p1_19 = (sum (extractDigitsRevRecur i1) + sum (extractDigitsRevRecur j1)) <= 19
											p2_19 = (sum (extractDigitsRevRecur i2) + sum (extractDigitsRevRecur j2)) <= 19

--Digit Helpers
extractDigitsRevRecur :: Int -> [Int]
extractDigitsRevRecur 0 = []
extractDigitsRevRecur n = (n `mod` 10) : (extractDigitsRevRecur (n `div` 10))

-- Union-Find
-- Disjoint set data type (weighted and using path compression).
-- O((M+N)lg*N + 2MlogN) worst-case union time (practically O(1))
-- For M union operations on a set of N elements.
-- O((M+N)lg*N) worst-case find time (practically O(1))
-- For M connected(find) operations on a set of N elements.
data DisjointSet = DisjointSet
     { count :: Int, ids :: (Seq Int), sizes :: (Seq Int) }
     deriving (Read,  Show)

-- Return id of root object
findRoot :: DisjointSet -> Int -> Int
findRoot set p | p == parent = p
               | otherwise   = findRoot set parent
               where
                parent = index (ids set) (p - 1)

-- Are objects P and Q connected ?
connected :: DisjointSet -> Int -> Int -> Bool
connected set p q = (findRoot set p) == (findRoot set q)

-- Replace sets containing P and Q with their union
quickUnion :: DisjointSet -> Int -> Int -> DisjointSet
quickUnion set p q | i == j = set
                   | otherwise = DisjointSet cnt rids rsizes
                     where
                        (i, j)   = (findRoot set p, findRoot set q)
                        (i1, j1) = (index (sizes set) (i - 1), index (sizes set) (j - 1))
                        (cnt, psmaller, size) = (count set - 1, i1 < j1, i1 + j1)
                        -- Always make smaller root point to the larger one
                        (rids, rsizes) = if psmaller
                                         then (update (i - 1) j (ids set), update (j - 1) size (sizes set))
                                         else (update (j - 1) i (ids set), update (i - 1) size (sizes set))


createUnions :: DisjointSet -> [(Int, Int)] -> DisjointSet
createUnions set [] = set
createUnions set ((p,q):xs) = createUnions (quickUnion set p q) xs

-- Main entry point for testing
main :: IO ()
main = do
    args <- getArgs
    let cnt1 = (read (head args) :: Int)
        cnt  = if (cnt1 < 10) then 10 else cnt1
        in do
           let set = (DisjointSet cnt (fromList [1, 2..cnt]) (replicate cnt 1))
               in do
                  putStr ("\ncreating union find with " ++ (show cnt) ++ " objects ...")
                  putStrLn ("DONE\n" ++ (show set))
                  putStrLn ("All objects are disconnected.")
                  putStrLn ("1 and 9 connected ? " ++ (show (connected set 1 9)))
                  putStrLn ("4 and 6 connected ? " ++ (show (connected set 4 6)))
                  putStrLn ("3 and 1 connected ? " ++ (show (connected set 3 1)))
                  putStrLn ("7 and 8 connected ? " ++ (show (connected set 7 8)))
                  putStr ("\ncreating unions ...")
                  let nset = (createUnions set [(4,1), (8,2), (7,3), (8,5), (3,4), (5,9), (5,1), (10,4), (6,1)])
                      in do
                         putStrLn ("DONE\n" ++ (show nset))
                         putStrLn ("All objects are connected (only 1 group).")
                         putStrLn ("1 and 9 connected ? " ++ (show (connected nset 1 9)))
                         putStrLn ("4 and 6 connected ? " ++ (show (connected nset 4 6)))
                         putStrLn ("3 and 1 connected ? " ++ (show (connected nset 3 1)))
                         putStrLn ("7 and 8 connected ? " ++ (show (connected nset 7 8)))


import Text.Regex.Posix

main  = do
			content <- getContents
			let linesOfFile = lines content
			let l1 = [getAllTextMatches (lineOfFile =~ pat :: AllTextMatches [] String) | lineOfFile <- linesOfFile]
			let l2 = [map (\x -> read x::Double) lineOfFile | lineOfFile <- l1]
			let l3 = [distance (lineOfFile !! 0) (lineOfFile !! 1) (lineOfFile !! 2) (lineOfFile !! 3) | lineOfFile <- l2]
			let outputLines = [putStr (show (round lineOfFile)) | lineOfFile <- l3]
			seqn outputLines

seqn :: [IO a] -> IO ()
seqn [] = return ()
seqn (x : xs) = do
					x; putChar '\n'
					seqn xs

pat = "[+|-]*[[:digit:]][[:digit:]]*"

distance :: Double -> Double -> Double -> Double -> Double
distance x1 y1 x2 y2 = sqrt ((x1 - x2)^2 + (y1 - y2)^2)

import qualified Data.Map as Map

main  = do
			content <- getContents
			let linesofFile = lines content
			let splits1 = [split lineOfFile ';' | lineOfFile <- linesofFile]
			let splits2 = [map (\x -> (digits Map.! x)) s1 | s1 <- splits1]
			let outputLines = [putStr y | y <- splits2]
			seqn outputLines

seqn :: [IO a] -> IO ()
seqn [] = return ()
seqn (x : xs) = do
					x; putChar '\n'
					seqn xs

split :: String -> Char -> [String]
split [] delim = [""]
split (x:xs) delim
	| x == delim = "" : rest
	| otherwise = (x : head rest) : tail rest
	where
		rest = split xs delim

digits :: Map.Map [Char] Char
digits = Map.fromList [ ( "zero" , '0' ) , ( "one" , '1' ) , ( "two" , '2' ) , ( "three" , '3' ) , ( "four" , '4' ) , ( "five" , '5' ) , ( "six" , '6' ) , ( "seven" , '7' ) , ( "eight" , '8' ) , ( "nine" , '9' )]
-}


{-
main  = do
			inpStr <- getContents
			let l1 = foldl (\pre cur -> maximizeRow pre (map (\x -> read x::Int) (words cur)) 0) [] (lines inpStr)
			let outputLines = [putStr (show (maximum l1))]
			seqn outputLines

seqn :: [IO a] -> IO ()
seqn [] = return ()
seqn (x : xs) = do
					x; putChar '\n'
					seqn xs

maxTri :: [[Int]] -> Int
maxTri rs = maximum (maximizeRows [] rs)

maximizeRows :: [Int] -> [[Int]] -> [Int]
maximizeRows r [] = r
maximizeRows r (r1:rs) = maximizeRows (maximizeRow r r1 0) rs

maximizeRow :: [Int] -> [Int] -> Int -> [Int]
maximizeRow [] cur n = [head cur + n]
maximizeRow (p:ps) (c:cs) n = (max (n + c) (p + c)) : (maximizeRow ps cs p)
-}
{-
main  = do
			content <- getContents
			let linesofFile = lines content
			let splits1 = [countDec lineOfFile | lineOfFile <- linesofFile]
			let outputLines = [putStr (show y) | y <- splits1]
			seqn outputLines

seqn :: [IO a] -> IO ()
seqn [] = return ()
seqn (x : xs) = do
					x; putChar '\n'
					seqn xs

countDec :: [Char] -> Int
countDec [] = 0
countDec [c] = 1
countDec [c1,c2] = if (read [c1,c2]::Int) > 26 then 1 else 2
countDec (c1:c2:cs) = if (read [c1,c2]::Int) > 26 then countDec (c2:cs) else (countDec cs) + (countDec $ c2:cs)
-}
{-
import Data.Array
import qualified Data.Map as Map
import Control.Monad.Fix

n :: Int
n = 10

testData = matrix 2 [4,6,2,8]
testData2 = matrix 3 [1..9]
testData4 = matrix 10 [1..100]
testData3 = matrix 100 [1..10000]

symbols :: Map.Map (Int,Int) Int
symbols = Map.fromList [((0,0),1), ((0,1), 2), ((1,0), 3), ((1,1), 4), ((2,0), 5), ((2,1), 6)]

matrix :: Int -> [Int] -> Array (Int,Int) Int
matrix n ds = listArray ((0,0),(n - 1, n - 1)) ds

nats :: Map.Map (Int,Int) Int
nats = Map.fromList [(((fst coord),(snd coord)),-1) | coord <- reverse $ range ((0,0),(n-1,n-1))]

calcMin_map :: Map.Map (Int,Int) Int
calcMin_map = fmap (calcMin fast_calcMin) nats

ind :: Map.Map (Int,Int) a -> Int -> a
ind mp i = mp Map.! (i `divMod` n)

toList :: Map.Map (Int,Int) Int -> [Int]
toList mp = map (ind mp) [0..99]

fast_calcMin :: Int -> Int
fast_calcMin = ind calcMin_map

calcMin :: (Int -> Int) -> Int -> Int
calcMin f l	| i == n - 1 && j == n - 1= testData4 ! (i,j)
			| i == n - 1              = testData4 ! (i,j) + f (i * n + (j + 1))
			|               j == n - 1= testData4 ! (i,j) + f ((i+1) * n + j)
			| otherwise			   	  = testData4 ! (i,j) + min (f (i * n + (j + 1))) (f ((i+1) * n + j))
			where
				(i,j) = l `divMod` n


--callAllCalcMin :: [Int] -> Int -> [Int]
--callAllCalcMin nums n = map (\p -> calcMinMemoized n arr (fst p) (snd p)) pairs
--						where
--							arr = matrix n nums
--							pairs = reverse $ range ((0,0),(n-1,n-1))

--calcMinMemoized :: Int -> Array (Int, Int) Int -> Int -> Int -> Int
--calcMinMemoized n arr i j

-}
{-
calcMin xs ys = a!(0,0)
	where
		n = length xs
		m = length ys
		a = array ((0,0),(n,m)) $ l1 ++ l2 ++ l3
		l1 = [((i,m),[]) | i <- [0..n]]
		l2 = [((n,j),[]) | j <- [0..m - 1]]
		l3 = [((i,j), f x y i j) | (x,i) <- zip xs [0..], (y,j) <- zip ys [0..]]
		f x y i j 
			| x == y    = x : a!(i+1,j+1)
			| otherwise = maxl (a!(i,j+1)) (a!(i+1,j))
-}


--calcMinMemoized :: Int -> Array (Int, Int) Int -> Int -> Int -> Int
--calcMinMemoized n arr i j = (Map.fromList [(coord, calcMin calcMinMemoized n arr (fst coord) (snd coord)) | coord <- reverse $ range ((0,0),(n-1,n-1))]) Map.! (i,j)

{-

calcMin :: Int -> Array (Int, Int) Int -> Int -> Int -> Int
calcMin n arr i j 	| i == n - 1 && j == n - 1= arr ! (i,j)
					| i == n - 1              = arr ! (i,j) + calcMin n arr i (j+1)
					|               j == n - 1= arr ! (i,j) + calcMin n arr (i+1) j
					| otherwise			   	  = arr ! (i,j) + min (calcMin n arr i (j+1)) (calcMin n arr (i+1) j)

main  = do
			content <- getContents
			let linesofFile = lines content
			let splits1 = [split lineOfFile '|' | lineOfFile <- linesofFile]
			let splits2 = [decode (head s1) (map (\w -> read w::Int) (words (head (tail s1)))) | s1 <- splits1]
			let outputLines = [putStr y | y <- splits2]
			seqn outputLines

seqn :: [IO a] -> IO ()
seqn [] = return ()
seqn (x : xs) = do
					x; putChar '\n'
					seqn xs

split :: String -> Char -> [String]
split [] delim = [""]
split (x:xs) delim
	| x == delim = "" : rest
	| otherwise = (x : head rest) : tail rest
	where
		rest = split xs delim
 
decode :: [Char] -> [Int] -> [Char]
decode _ [] = []
decode cs (d:ds) = (cs !! (d - 1)) : decode cs ds

main  =
        getContents >>= \content ->
		let linesofFile = lines content
		in let splits1 = [isHappy (read lineOfFile::Int) | lineOfFile <- linesofFile]
		in let outputLines = [putStr (if y then "1" else "0") | y <- splits1]
		in seqn outputLines

seqn :: [IO a] -> IO ()
seqn [] = return ()
seqn (x : xs) = do
					x; putChar '\n'
					seqn xs

isHappy n = (last (take 100 (sumSumDigs n))) == 1

sumSumDigs n = n : sumSumDigs (sum [x^2|x<-(extractDigitsRev n)])

extractDigitsRev :: Int -> [Int]
extractDigitsRev 0 = [0]
extractDigitsRev n = extractDigitsRevRecur n

extractDigitsRevRecur :: Int -> [Int]
extractDigitsRevRecur 0 = []
extractDigitsRevRecur n = (n `mod` 10) : (extractDigitsRevRecur (n `div` 10))

{-
memoized_minSum :: Int -> Int -> Int -> Int
memoized_minSum n i j = (map minSum [0 ..] !!)
   where minSum  n i j 	| i == n - 1 && j == n - 1	= testData ! (i,j)
						| i == n - 1				= testData ! (i,j) + memoized_minSum 
				|               j == n - 1	= do
												mem <- minSum arr n (i + 1) j
												return (arr ! (i,j) + mem)
				| otherwise					= do
												mem1 <- minSum arr n i (j + 1)
												mem2 <- minSum arr n (i + 1) j
												return (arr ! (i,j) + min mem1 mem2)
-}

--calcAll :: Int -> [Int] -> Int
--calcAll n ds = min (calcMin) ()head [calcMin n arr mp (fst coord) (snd coord) | coord <- reverse $ range ((0,0),(n-1,n-1))]
--				where
--					arr = matrix n ds

--calcMin :: Int -> Array (Int, Int) Int -> Map.Map (Int,Int) Int -> Int -> Int -> Map.Map (Int,Int) Int
--calcMin n arr mp i j | i == n - 1 && j == n -1 = Map.insert (i,j) (arr ! (i,j)) mp
--					 | i == n - 1              = Map.insert (i,j) (arr ! (i,j) + mp Map.! (i,j+1)) mp
--					 |              j == n - 1 = Map.insert (i,j) (arr ! (i,j) + mp Map.! (i+1,j)) mp
--					 | otherwise			   = Map.insert (i,j) (arr ! (i,j) + min (mp Map.! (i,j+1)) (mp Map.! (i+1,j))) mp
-}
{-
import Data.Array

val :: Array (Int,Int) Char
val = listArray ((0,0),(2,4)) ['a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l', 'm', 'n', 'o']
main  =
        getContents >>= \content ->
		let linesofFile = lines content
		in let splits1 = [split lineOfFile ';' | lineOfFile <- linesofFile]
		in let splits2 = [solve (read (head s1)::Int) (read (head (tail s1))::Int) (split (head (tail (tail s1))) ' ') | s1 <- splits1]
		in let outputLines = [putStr (foldResult y) | y <- splits2]
		in seqn outputLines

foldResult :: [[Char]] -> [Char]
foldResult [] = ""
foldResult [x] = x
foldResult (x:xs) = x ++ " " ++ foldResult xs

seqn :: [IO a] -> IO ()
seqn [] = return ()
seqn (x : xs) = do
					x; putChar '\n'
					seqn xs

split :: String -> Char -> [String]
split [] delim = [""]
split (x:xs) delim
	| x == delim = "" : rest
	| otherwise = (x : head rest) : tail rest
	where
		rest = split xs delim
 
solve :: Int -> Int -> [a] -> [a]
solve n m ds = sideWalks arr n m 0 0
				where
					arr = listArray ((0,0),(n - 1, m - 1)) ds

sideWalks :: Array (Int, Int) a -> Int -> Int -> Int -> Int -> [a]
sideWalks _   0 _ _ _ = []
sideWalks _   _ 0 _ _ = []
sideWalks arr 1 1 x y = [arr ! (x,y)]
sideWalks arr n 1 x y = [arr ! (x + i,y) | i <- [0..n - 1]]
sideWalks arr 1 m x y = [arr ! (x,y + i) | i <- [0..m - 1]]
sideWalks arr n m x y = sideWalk arr n m x y ++ sideWalks arr (n - 2) (m - 2) (x + 1) (y + 1)

sideWalk :: Array (Int, Int) a -> Int -> Int -> Int -> Int -> [a]
sideWalk arr n m x y = [arr ! (x,y + i) | i <- [0..m - 1]] ++
					   [arr ! (x + i,y + m - 1) | i <- [1..n - 1]] ++
					   [arr ! (x + n - 1,y + i) | i <- reverse [0..m - 2]] ++
					   [arr ! (x + i,y) | i <- reverse [1..n - 2]]

import Data.Array

main  =
        getContents >>= \content ->
		let linesofFile = lines content
		in let splits1 = [split lineOfFile ';' | lineOfFile <- linesofFile]
		in let splits2 = [validate (read (head s1)::Int) (map (\x -> read x::Int) (split (head (tail s1)) ',')) | s1 <- splits1]
		in let outputLines = [putStr (show y) | y <- splits2]
		in seqn outputLines

seqn :: [IO a] -> IO ()
seqn [] = return ()
seqn (x : xs) = do
					x; putChar '\n'
					seqn xs


split :: String -> Char -> [String]
split [] delim = [""]
split (x:xs) delim
	| x == delim = "" : rest
	| otherwise = (x : head rest) : tail rest
	where
		rest = split xs delim
 

validate :: Int -> [Int] -> Bool
validate n items = (areEqual cs) && (areEqual rs) && (areEqual bs)
					where
						val = sudoku n items
						cs = itemSums n val colSum
						rs = itemSums n val rowSum
						bs = blockSums n val
					
sudoku :: Int -> [Int] -> Array (Int,Int) Int
sudoku n ds = listArray ((0,0),(n - 1, n - 1)) ds

areEqual :: [Int] -> Bool
areEqual [] = True
areEqual [a] = True
areEqual (x1:x2:xs) = if x1 /= x2 then False else areEqual (x2:xs)

itemSums :: Int -> Array (Int, Int) Int -> (Int -> Int -> Array (Int,Int) Int -> Int) -> [Int]
itemSums n arr itemSum = [itemSum n i arr | i <- [0.. n - 1]]

colSum :: Int -> Int -> Array (Int,Int) Int -> Int
colSum n col arr = sum [arr ! (i,col) | i <- [0..n-1]]

rowSum :: Int -> Int -> Array (Int,Int) Int -> Int
rowSum n row arr = sum [arr ! (row,i) | i <- [0..n-1]]

blockSums :: Int -> Array (Int,Int) Int -> [Int]
blockSums n arr = [blockSum block x y arr | x <- bs, y <- bs]
					where
						block = truncate $ sqrt $ fromIntegral n
						bs = [0, block..(n - block)]
					
blockSum :: Int -> Int -> Int -> Array (Int,Int) Int -> Int
blockSum b x y arr = sum [arr ! (j,i) | i <- [y..y + b - 1], j <- [x..x + b -1]]

main  =
        getContents >>= \content ->
		let linesofFile = lines content
		in let splits1 = [poppop (foldl push [] (map (\x -> read x::Int) (words lineOfFile))) | lineOfFile <- linesofFile]
		in let outputLines = [putStr (foldResult (map (\x -> show x) y)) | y <- splits1]
		in seqn outputLines

seqn :: [IO a] -> IO ()
seqn [] = return ()
seqn (x : xs) = do
					x; putChar '\n'
					seqn xs

foldResult :: [[Char]] -> [Char]
foldResult [] = ""
foldResult [x] = x
foldResult (x:xs) = x ++ " " ++ foldResult xs

poppop :: [Int] -> [Int]
poppop [] = []
poppop [n] = [n]
poppop (n1:n2:ns) = n1 : poppop ns

push :: [Int] -> Int -> [Int]
push ns n = n : ns

import Control.Monad.Fix

main  =
        getContents >>= \content ->
		let linesofFile = lines content
		in let splits1 = [read lineOfFile::Int | lineOfFile <- linesofFile]
		in let outputLines = [putStr (show (fibMemo y)) | y <- splits1]
		in seqn outputLines

seqn :: [IO a] -> IO ()
seqn [] = return ()
seqn (x : xs) = do
					x; putChar '\n'
					seqn xs

fib :: (Int -> Integer) -> Int -> Integer
fib f 0 = 1
fib f 1 = 1
fib f n = f (n - 1) + f (n - 2)

memoize :: (Int -> a) -> (Int -> a)
memoize f = (map f [0 ..] !!)

fibMemo :: Int -> Integer
fibMemo = fix (memoize . fib)

import Data.Array

main  =
        getContents >>= \content ->
		let linesofFile = lines content
		in let splits1 = [split x ';' | x <- linesofFile]
		in let splits2 = [ sumAllMines (read (head (split (head split1) ','))::Int) (read (head (tail (split (head split1) ',')))::Int) (head (tail split1)) | split1 <- splits1]
		in let outputLines = [putStr (foldResult y) | y <- splits2]
		in seqn outputLines

foldResult :: [[Char]] -> [Char]
foldResult [] = ""
foldResult [x] = x
foldResult (x:xs) = x ++ foldResult xs

seqn :: [IO a] -> IO ()
seqn [] = return ()
seqn (x : xs) = do
					x; putChar '\n'
					seqn xs

split :: String -> Char -> [String]
split [] delim = [""]
split (x:xs) delim
	| x == delim = "" : rest
	| otherwise = (x : head rest) : tail rest
	where
		rest = split xs delim
 
sumAllMines :: Int -> Int -> [Char] -> [[Char]]
sumAllMines m n ds = map (sumMines m n (mineField m n ds)) [0..m*n - 1] 

mineField :: Int -> Int -> [Char] -> Array Int Char
mineField m n ds = listArray (0,m*n - 1) ds

isMine :: Int -> Int -> Int -> Int -> Array Int Char -> Int
isMine m n i j fld	| i < 0 || i >= m = 0
					| j < 0 || j >= n = 0
					| otherwise = if fld ! (i * n + j) == '*' then 1 else 0

sumMines :: Int -> Int -> Array Int Char -> Int -> [Char]
sumMines m n fld i =if fld!i == '*' then "*" else show ((isMine m n (row-1) (col-1) fld)
												+ (isMine m n (row-1) (col-0) fld)
												+ (isMine m n (row-1) (col+1) fld)
												+ (isMine m n (row-0) (col-1) fld)
												+ (isMine m n (row-0) (col+1) fld)
												+ (isMine m n (row+1) (col-1) fld)
												+ (isMine m n (row+1) (col-0) fld)
												+ (isMine m n (row+1) (col+1) fld))
					where
						row = i `div` n
						col = i `mod` n
						
import Data.Array

main  =
        getContents >>= \content ->
		let linesofFile = lines content
		in let splits1 = [split x ';' | x <- linesofFile]
		in let splits2 = [ longestCommon (head split1) (head (tail split1)) | split1 <- splits1]
		in let outputLines = [putStr y | y <- splits2]
		in seqn outputLines

seqn :: [IO a] -> IO ()
seqn [] = return ()
seqn (x : xs) = do
					x; putChar '\n'
					seqn xs

split :: String -> Char -> [String]
split [] delim = [""]
split (x:xs) delim
	| x == delim = "" : rest
	| otherwise = (x : head rest) : tail rest
	where
		rest = split xs delim
 
longestCommon xs ys = a!(0,0)
	where
		n = length xs
		m = length ys
		a = array ((0,0),(n,m)) $ l1 ++ l2 ++ l3
		l1 = [((i,m),[]) | i <- [0..n]]
		l2 = [((n,j),[]) | j <- [0..m - 1]]
		l3 = [((i,j), f x y i j) | (x,i) <- zip xs [0..], (y,j) <- zip ys [0..]]
		f x y i j 
			| x == y    = x : a!(i+1,j+1)
			| otherwise = maxl (a!(i,j+1)) (a!(i+1,j))

maxl str1 str2 = if (length str1) > (length str2) then str1 else str2

--memo :: ([Char] -> [Char] -> a) -> [[a]]
--memo f = map (\x -> map (f x) [0..]) [0..]

gw :: [Char] -> [Char] -> [Char] -> [Char] -> Int
gw _ _ [] lst = 0
gw _ _ lst [] = 0
gw str1 str2 (x:xs) (y:ys) = if x == y then 1 + (fastgw str1 str2 xs ys) else max (fastgw str1 str2 (x:xs) ys) (fastgw str1 str2 xs (y:ys))

test1 str1 = (\x -> map (gw str1 str1 (drop x str1)) [0..])

gwstore :: [Char] -> [Char] -> [[Int]]
gwstore str1 str2 = [[1]]
-- \y -> map ( (drop y str2)) [0..])
-- \y -> map  [0..]

fastgw :: [Char] -> [Char] -> [Char] -> [Char] -> Int
fastgw str1 str2 x y = (gwstore str1 str2) !! (length x) !! (length y)
-}
{-
import Control.Monad.Fix

longestCommon :: ([Char] -> [Char] -> Int) -> [Char] -> [Char] -> Int
longestCommon f [] lst = 0
longestCommon f lst [] = 0
longestCommon f (c:cs) (d:ds) = if c == d then (1 + f cs ds) else max (f cs (d:ds)) (f (c:cs) ds)

--longestCommonMemo :: [Char] -> [Char] -> Int
--longestCommonMemo = fix (memoize . longestCommon)

memo :: (Num a, Enum a) => (a -> b) -> [b]
memo f = map f (enumFrom 0)

--memo2 :: (Num a, Enum a) => (a -> b) -> [b]
memo2 f = map memo (memo f)

--memoize :: ([Char] -> [Char] -> a) -> ([Char] -> [Char] -> a)
--memoize f = memoAtom (memoAtom f)


memoize :: (Int -> Int -> a) -> [[a]]

lst :: [Char] -> [Char] -> Int
lst [] s = 0
lst s [] = 0
lst (c:cs) (d:ds) = if c == d then 1 + (fastlst cs ds) else max (fastlst (c:cs) ds) (fastlst cs (d:ds))

lststore :: [[Int]]
lststore = memoize lst

fastlst :: [Char] -> [Char] -> Int
fastlst x y = lststore !! x !! y
-}
{-
longestCommonMemo :: Int -> Integer
longestCommonMemo = fix (memoize . longestCommon)

longestCommon :: ([Char] -> [Char] -> Int) -> [Char] -> [Char] -> Int
longestCommon f [] lst = 0
longestCommon f lst [] = 0
--longestCommon f (c:cs) (d:ds) = if c == d then (1 + longestCommon cs ds) else (max (longestCommon cs (d:ds)) (longestCommon (c:cs) ds))

main  =
        getContents >>= \content ->
		let linesofFile = lines content
		in let splits1 = [split x ';' | x <- linesofFile]
		in let splits2 = [ longestCommon (head split1) (head (tail split1)) | split1 <- splits1]
		in let outputLines = [putStr y | y <- splits2]
		in seqn outputLines

seqn :: [IO a] -> IO ()
seqn [] = return ()
seqn (x : xs) = do
					x; putChar '\n'
					seqn xs

split :: String -> Char -> [String]
split [] delim = [""]
split (x:xs) delim
	| x == delim = "" : rest
	| otherwise = (x : head rest) : tail rest
	where
		rest = split xs delim

longestCommon :: [Char] -> [Char] -> [Char]
longestCommon [] lst = []
longestCommon lst [] = []
longestCommon (c:cs) (d:ds) = if c == d then (c : longestCommon cs ds) else (maxSize (longestCommon cs (d:ds)) (longestCommon (c:cs) ds))

maxSize :: [Char] -> [Char] -> [Char]
maxSize str1 str2 = if (length str1) > (length str2) then str1 else str2

import Control.Monad.Fix

--(.) :: (b -> c) -> (a -> b) -> a -> c
--(.) f g = \ x -> f (g x)
 
--fix :: (a -> a) -> a
--fix f = let x = f x in x

fib :: (Int -> Integer) -> (Int -> Integer)
fib f 0 = 1
fib f 1 = 1
fib f n = f (n - 1) + f (n - 2)

memoize :: (Int -> a) -> (Int -> a)
memoize f = (map f [0 ..] !!)

fibMemo :: Int -> Integer
fibMemo = fix (memoize . fib)

findGreater :: Int -> Int -> Int
findGreater m n = if n >= m then n else findGreater m (n * 2)

main  =
        getContents >>= \content ->
		let outputLines = [putStr (show (length content))]
		in seqn outputLines

seqn :: [IO a] -> IO ()
seqn [] = return ()
seqn (x : xs) = do
					x; putChar '\n'
					seqn xs

import Data.Map hiding (map)

main  =
        getContents >>= \content ->
		let linesofFile = lines content
		in let splits1 = [map (\item -> words item) (split1 x) | x <- linesofFile]
		in let splits2 = [ translateWords s | s <- splits1]
		in let outputLines = [putStr (foldResult y) | y <- splits2]
		in seqn outputLines

foldResult :: [[Char]] -> [Char]
foldResult [] = ""
foldResult [x] = x
foldResult (x:xs) = x ++ " " ++ foldResult xs

seqn :: [IO a] -> IO ()
seqn [] = return ()
seqn (x : xs) = do
					x; putChar '\n'
					seqn xs

split1 :: String -> [String]
split1 [] = [""]
split1 [x] = [[x]]
split1 (x:xs)
	| x == ' ' && (head xs) == ' ' = "" : rest
	| otherwise = (x : head rest) : tail rest
	where
		rest = split1 xs

translateWords :: [[[Char]]] -> [[Char]]
translateWords [] = []
translateWords (ws:wss) = translateWord ws : translateWords wss

translateWord :: [[Char]] -> [Char]
translateWord [] = []
translateWord (w:ws) = symbols ! w : translateWord ws

symbols :: Map [Char] Char
symbols = fromList [(".-",'A'), ("-...", 'B'), ("-.-.", 'C'), ("-..", 'D'), (".", 'E'), ("..-.", 'F'), ("--.", 'G'), ("....", 'H'), ("..", 'I'), (".---", 'J'), ("-.-", 'K'), (".-..", 'L'), ("--", 'M'), ("-.", 'N'), ("---", 'O'), (".--.", 'P'), ("--.-", 'Q'), (".-.", 'R'), ("...", 'S'), ("-", 'T'), ("..-", 'U'), ("...-", 'V'), (".--", 'W'), ("-..-", 'X'), ("-.--", 'Y'), ("--..", 'Z'), (".----", '1'), ("..---", '2'), ("...--", '3'), ("....-", '4'), (".....", '5'), ("-....", '6'), ("--...", '7'), ("---..", '8'), ("----.", '9'), ("-----", '0')]

main  =
        getContents >>= \content ->
		let linesofFile = lines content
		in let splits1 = [words lineOfFile | lineOfFile <- linesofFile]
		in let splits2 = [ longestRecur split1 "" | split1 <- splits1]
		in let outputLines = [putStr y | y <- splits2]
		in seqn outputLines

seqn :: [IO a] -> IO ()
seqn [] = return ()
seqn (x : xs) = do
					x; putChar '\n'
					seqn xs

longestRecur :: [[Char]] -> [Char] -> [Char]
longestRecur [] max = max
longestRecur (item:items) max = if (length item) > (length max) then longestRecur items item else longestRecur items max

main  =
        getContents >>= \content ->
		let linesofFile = lines content
		in let splits1 = [map (\item -> split item ',') (split x ';') | x <- linesofFile]
		in let splits2 = [ foldl (\z x -> if x == [""] then z else (read (head(tail x))::Int) : z) [] split1 | split1 <- splits1]
		in let result = [ sortDiff split2 | split2 <- splits2]
		in let outputLines = [if y == [] then putStr "" else (putStr (foldResult y)) | y <- result]
		in seqn outputLines

foldResult :: [Int] -> [Char]
foldResult [] = ""
foldResult [x] = (show x)
foldResult (x:xs) = (show x) ++ "," ++ foldResult xs

seqn :: [IO a] -> IO ()
seqn [] = return ()
seqn (x : xs) = do
					x; putChar '\n'
					seqn xs

split :: String -> Char -> [String]
split [] delim = [""]
split (x:xs) delim
	| x == delim = "" : rest
	| otherwise = (x : head rest) : tail rest
	where
		rest = split xs delim

sortDiff :: [Int] -> [Int]
sortDiff [] = []
sortDiff [x] = [x]
sortDiff lst = head slist : (diff slist)
				where 
					slist = qsort lst

diff :: [Int] -> [Int]
diff [] = []
diff [x,y] = [y-x]
diff (x:xs) = (head xs - x) : diff xs

qsort [] = []
qsort (c:cs) = qsort smaller ++ [c] ++ qsort larger
				where
					smaller = [x | x <- cs, x <= c]
					larger = [x | x <- cs, x > c]

main  =
        getContents >>= \content ->
		let linesofFile = lines content
		in let numLinePairs = [(split x ';') | x <- linesofFile]
		in let seqCount = [ interset (map (\s -> read s::Int) (split (head numLinePair) ',')) (map (\s -> read s::Int) (split (head (tail numLinePair)) ',')) | numLinePair <- numLinePairs]
		in let outputLines = [if y == [] then putStr "" else (putStr (foldResult y)) | y <- seqCount]
		in seqn outputLines

foldResult :: [Int] -> [Char]
foldResult [] = ""
foldResult [x] = (show x)
foldResult (x:xs) = (show x) ++ "," ++ foldResult xs

seqn :: [IO a] -> IO ()
seqn [] = return ()
seqn (x : xs) = do
					x; putChar '\n'
					seqn xs

split :: String -> Char -> [String]
split [] delim = [""]
split (x:xs) delim
	| x == delim = "" : rest
	| otherwise = (x : head rest) : tail rest
	where
		rest = split xs delim

interset :: [Int] -> [Int] -> [Int]
interset [] _ = []
interset _ [] = []
interset (x:xs) (y:ys) = case compare x y of
							EQ -> x : interset xs ys
							LT -> interset xs (y:ys)
							GT -> interset (x:xs) ys

import Data.Array

getDataArrayItems :: Int -> Int -> Array Int Char -> [Char]
getDataArrayItems row col items = if inRange (bounds items) (row * col) then 

dataArray :: Int -> Int -> [Char] -> Array Int Char
dataArray rows cols entries = listArray (0, rows * cols - 1) entries

--getCoords :: Int -> Int -> [(Int, Int)]
--getCoords row col | row == 0 && col == 0 = [(

main  =
        getContents >>= \content ->
		let linesofFile = lines content
		in let numLinePairs = [(split x '|') | x <- linesofFile]
		in let seqCount = [zipWith (*) (map (\s -> read s::Int) (words (head numLinePair))) (map (\s -> read s::Int) (words (head (tail numLinePair))))| numLinePair <- numLinePairs]
		in let outputLines = [putStr (foldResult y) | y <- seqCount]
		in seqn outputLines

foldResult :: [Int] -> [Char]
foldResult [n] = show n
foldResult (n:ns) = (show n) ++ " " ++ foldResult ns

seqn :: [IO a] -> IO ()
seqn [] = return ()
seqn (x : xs) = do
					x; putChar '\n'
					seqn xs

split :: String -> Char -> [String]
split [] delim = [""]
split (x:xs) delim
	| x == delim = "" : rest
	| otherwise = (x : head rest) : tail rest
	where
		rest = split xs delim

import Data.Array

main  =
        getContents >>= \content ->
		let linesofFile = lines content
		in let numLinePairs = [(split x ';') | x <- linesofFile]
		in let seqCount = [ searchSums (map (\s -> read s::Int) (split (head numLinePair) ',')) (read (head (tail numLinePair))::Int) | numLinePair <- numLinePairs]
		in let outputLines = [if y == [] then putStr "NULL" else (putStr (foldResult y)) | y <- seqCount]
		in seqn outputLines

foldResult :: [(Int,Int)] -> [Char]
foldResult [] = ""
foldResult [(x,y)] = (show x) ++ "," ++ (show y)
foldResult ((x,y):xs) = (show x) ++ "," ++ (show y) ++ ";" ++ foldResult xs

seqn :: [IO a] -> IO ()
seqn [] = return ()
seqn (x : xs) = do
					x; putChar '\n'
					seqn xs

split :: String -> Char -> [String]
split [] delim = [""]
split (x:xs) delim
	| x == delim = "" : rest
	| otherwise = (x : head rest) : tail rest
	where
		rest = split xs delim

searchSums :: [Int] -> Int -> [(Int,Int)]
searchSums [] _ = []
searchSums (n:ns) s = case searchItem items (s - n) of
						Nothing -> searchSums ns s
						Just x -> (n, items ! x) : searchSums ns s
						where
							items = (searchDomain ((length ns) - 1) ns)

searchItem :: Array Int Int -> Int -> Maybe Int
searchItem items item = binarySearchArray items item

searchDomain count items = listArray (0,count) items
 
binarySearchArray :: (Ix i, Integral i, Ord e) => Array i e -> e -> Maybe i
binarySearchArray a x = binarySearch p (bounds a)
							where
								p m = x `compare` (a ! m)
  
binarySearch :: Integral a => (a -> Ordering) -> (a, a) -> Maybe a
binarySearch p (low,high) 
  | high < low = Nothing
  | otherwise = 
      let mid = (low + high) `div` 2 in 
      case p mid of
        LT -> binarySearch p (low, mid-1)
        GT -> binarySearch p (mid+1, high)
        EQ -> Just mid

import Data.Array

rowCount :: Int -> Array Int Int -> [Int]

board :: (Ix a, Int a) => a -> [Int] -> Array a Int
board n nums = listArray (0,n*n - 1) nums

main  =
        getContents >>= \content ->
		let linesofFile = lines content
		in let nums = [(split x ',') | x <- linesofFile]
		in let seqCount = [trimCharsRecur (head cs) (remChars (trim (head (tail cs)))) | cs <- nums]
		in let outputLines = [putStr y | y <- seqCount]
		in seqn outputLines

seqn :: [IO a] -> IO ()
seqn [] = return ()
seqn (x : xs) = do
					x; putChar '\n'
					seqn xs

split :: String -> Char -> [String]
split [] delim = [""]
split (x:xs) delim
	| x == delim = "" : rest
	| otherwise = (x : head rest) : tail rest
	where
		rest = split xs delim

trimCharsRecur :: [Char] -> Array Int Bool -> [Char]
trimCharsRecur [] _ = []
trimCharsRecur (c:cs) ds = if (ds ! (fromEnum c)) then trimCharsRecur cs ds else (c : trimCharsRecur cs ds)

genCharSeq :: [Char] -> [Bool]
genCharSeq [] = [ False | _ <- [0..255]]
genCharSeq lst = genSeq (map (\c -> fromEnum c) lst)

genSeq :: [Int] -> [Bool]
genSeq lst = [False | _ <- [0..(head lst) - 1]] ++ genSeqReq lst

genSeqReq :: [Int] -> [Bool]
genSeqReq [x] = genRange x 255
genSeqReq (x:xs) = (genRange x ((head xs) - 1)) ++ genSeqReq xs

genRange :: Int -> Int -> [Bool]
genRange n m = True : [False | x <- [n + 1..m]]

qsort [] = []
qsort (c:cs) = qsort smaller ++ [c] ++ qsort larger
				where
					smaller = [x | x <- cs, x <= c]
					larger = [x | x <- cs, x > c]

remChars chars = listArray (0,255) (genCharSeq chars)

trim :: [Char] -> [Char]
trim [] = []
trim (c:cs) = if c == ' ' then trim cs else c : trim cs

main  =
        getContents >>= \content ->
		let linesofFile = lines content
		in let nums = [read lineOfFile::Int | lineOfFile <- linesofFile]
		in let outputLines = [putStr (show (sum nums))]
		in seqn outputLines

seqn :: [IO a] -> IO ()
seqn [] = return ()
seqn (x : xs) = do
					x; putChar '\n'
					seqn xs

nextToLast :: [[Char]] -> [Char]
nextToLast [] = []
nextToLast [_] = []
nextToLast lst = lst !! ((length lst) - 2)

main  =
        getContents >>= \content ->
		let linesOfFile = lines content
		in let swappedCaseLines = [swapCase lineOfFile | lineOfFile <- linesOfFile]
		in let outputLines = [putStr y | y <- swappedCaseLines]
		in seqn outputLines

seqn :: [IO a] -> IO ()
seqn [] = return ()
seqn (x : xs) = do
					x; putChar '\n'
					seqn xs

swapCase :: [Char] -> [Char]
swapCase [] = []
swapCase (c:cs) = swapCharCase c : swapCase cs

swapCharCase :: Char -> Char
swapCharCase c 	| ((fromEnum 'a') <= (fromEnum c)) && ((fromEnum c) <= (fromEnum 'z')) = (toEnum ((fromEnum c) - (fromEnum 'a') + (fromEnum 'A')))
				| ((fromEnum 'A') <= (fromEnum c)) && ((fromEnum c) <= (fromEnum 'Z')) = (toEnum ((fromEnum c) - (fromEnum 'A') + (fromEnum 'a')))
				| otherwise = c
				
import Data.List (insert)
import Text.Regex.Posix

pat = "(foo[a-z]*bar|quux)"

res = "i foobarbar a quux" =~ pat :: ([] (Int,Int))

isSquare :: Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Bool
isSquare x1 y1 x2 y2 x3 y3 x4 y4 = (a1 == a2)
								&& (a3 == a4)
								&& (b1 == b3)
								&& (b2 == b4)
								&& ((a3 - a1) == (b4 - b3))
									where
										lst = insertionSort[(x1,y1),(x2,y2),(x3,y3),(x4,y4)]
										(a1,b1) = head lst
										(a2,b2) = head (tail lst)
										(a3,b3) = head (tail (tail lst))
										(a4,b4) = head (tail (tail (tail lst)))

insertionSort :: Ord a => [a] -> [a]
insertionSort = foldr insert []

main = putStr (show primesSum)

primesSum :: Int
primesSum = sum (take 1000 primesRecur)

primesRecur :: [Int]
primesRecur = sieve [2..]
				where sieve (p:xs) = p : sieve [x|x <- xs, x `mod` p > 0]

main  =
        getContents >>= \content ->
		let linesofFile = lines content
		in let nums = [read lineOfFile::Int | lineOfFile <- linesofFile]
		in let seqCount = [map (\n -> show n) (primes num) | num <- nums]
		in let outputLines = [putStr (foldStrings y) | y <- seqCount]
		in seqn outputLines

foldStrings :: [[Char]] -> [Char]
foldStrings [] = ""
foldStrings (string:[]) = string
foldStrings (string:strings) = string ++ "," ++ (foldStrings strings)
		
seqn :: [IO a] -> IO ()
seqn [] = return ()
seqn (x : xs) = do
					x; putChar '\n'
					seqn xs

split :: String -> Char -> [String]
split [] delim = [""]
split (x:xs) delim
	| x == delim = "" : rest
	| otherwise = (x : head rest) : tail rest
	where
		rest = split xs delim

primes :: Int -> [Int]
primes n| n < 3 = []
		| otherwise = sieve [2..(n-1)]
						where
							sieve (p:[]) = [p]
							sieve (p:xs) = p : sieve [x|x <- xs, x `mod` p > 0]

primesBetween :: Int -> Int -> Int
primesBetween n m 	|(last nPrimes) == n = (length mPrimes) - (length nPrimes) + 1
					|otherwise = (length mPrimes) - (length nPrimes)
					where
						mPrimes = primes m
						nPrimes = primes n

main  =
        getContents >>= \content ->
		let linesofFile = lines content
		in let nums = [map (\str -> read str::Int) (split x ',') | x <- linesofFile]
		in let seqCount = [overlap (coords !! 0) (coords !! 1) (coords !! 2) (coords !! 3) (coords !! 4) (coords !! 5) (coords !! 6) (coords !! 7) | coords <- nums]
		in let outputLines = [putStr (show y) | y <- seqCount]
		in seqn outputLines

seqn :: [IO a] -> IO ()
seqn [] = return ()
seqn (x : xs) = do
					x; putChar '\n'
					seqn xs

split :: String -> Char -> [String]
split [] delim = [""]
split (x:xs) delim
	| x == delim = "" : rest
	| otherwise = (x : head rest) : tail rest
	where
		rest = split xs delim
	
overlap :: Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Bool
overlap x1 y1 x2 y2 a1 b1 a2 b2 = (pointInRectangle x1 y1 x2 y2 a1 b1)
								||(pointInRectangle x1 y1 x2 y2 a2 b2)
								||(pointInRectangle x1 y1 x2 y2 a1 b2)
								||(pointInRectangle x1 y1 x2 y2 a2 b1)
								||(pointInRectangle a1 b1 a2 b2 x1 y1)
								||(pointInRectangle a1 b1 a2 b2 x2 y2)
								||(pointInRectangle a1 b1 a2 b2 x1 y2)
								||(pointInRectangle a1 b1 a2 b2 x2 y1)

pointInRectangle :: Int -> Int -> Int -> Int -> Int -> Int -> Bool
pointInRectangle xl yl xr yr a b = (a >= xl) && (a <= xr) && (b <= yl) && (b >= yr)

main  =
        getContents >>= \content ->
		let linesofFile = lines content
		in let stringSubstringLines = [splitLine (split x ',') | x <- linesofFile]
		in let seqCount = [distinctSequenceCount (head stringPair) (head (tail stringPair)) | stringPair <- stringSubstringLines]
		in let outputLines = [putStr (show y) | y <- seqCount]
		in seqn outputLines

seqn :: [IO a] -> IO ()
seqn [] = return ()
seqn (x : xs) = do
					x; putChar '\n'
					seqn xs

splitLine :: [String] -> [String]
splitLine inputStrings = [x | x <- inputStrings]

split :: String -> Char -> [String]
split [] delim = [""]
split (x:xs) delim
	| x == delim = "" : rest
	| otherwise = (x : head rest) : tail rest
	where
		rest = split xs delim
					
distinctSequenceCount :: [Char] -> [Char] -> Int
distinctSequenceCount string [] = 0
distinctSequenceCount string seq = length (extractWholeString [string] seq)

extractWholeString :: [[Char]] -> [Char] -> [[Char]]
extractWholeString [[]] chars = []
extractWholeString [] chars = []
extractWholeString strings [] = strings
extractWholeString strings (c:cs) = extractWholeString (extractMultipleSubstrings strings c) cs

extractMultipleSubstrings :: [[Char]] -> Char -> [[Char]]
extractMultipleSubstrings [] char = []
extractMultipleSubstrings [[]] char = []
extractMultipleSubstrings (cs:css) char = (extractSubstrings cs char) ++ (extractMultipleSubstrings css char)

extractSubstrings :: [Char] -> Char -> [[Char]]
extractSubstrings [] char = []
extractSubstrings (c:cs) char	| c == char = cs : (extractSubstrings cs char)
								| True = (extractSubstrings cs char)

main  =
        getContents >>= \content ->
		let linesofFile = lines content
		in let nums = [process (words lineOfFile) | lineOfFile <- linesofFile]
		in let outputLines = [putStr (show num) | num <- nums ]
		in seqn outputLines

seqn :: [IO a] -> IO ()
seqn [] = return ()
seqn (x : xs) = do
					x; putChar '\n'
					seqn xs

items :: [([Char], Integer)]
items = [("zero",0), ("one",1), ("two",2), ("three",3), ("four",4), ("five",5), ("six",6), ("seven",7), ("eight",8), ("nine",9),
		 ("ten",10), ("eleven",11), ("twelve",12), ("thirteen",13), ("fourteen",14), ("fifteen",15), ("sixteen",16), ("seventeen",17), ("eighteen",18), ("nineteen",19),
		 ("twenty",20), ("thirty",30), ("forty",40), ("fifty",50), ("sixty",60), ("seventy",70), ("eighty",80), ("ninety",90)]
		 
hundred = "hundred"
thousand = "thousand"
million = "million"

process :: [[Char]] -> Integer
process [] = 0
process ("negative":xs) = -1 * (sum (processRecur xs []))
process lst = sum (processRecur lst [])

processRecur :: [[Char]] -> [Integer] -> [Integer]
processRecur [] temps = [sum temps]
processRecur (text:texts) temps | text == million = ((sum temps) * 1000000) : (processRecur texts [])
								| text == thousand = ((sum temps) * 1000) : (processRecur texts [])
								| text == hundred = processRecur texts [((sum temps) * 100)]
								| otherwise = processRecur texts ((getItemValue items text) : temps)

getItemValue :: [([Char], Integer)] -> [Char] -> Integer
getItemValue [] name = 0
getItemValue (r:rs) name = if (name == (fst r)) then (snd r) else (getItemValue rs name)

--Points in circle

isInside :: Double -> Double -> Double -> Double -> Double -> Bool
isInside cx cy r x y = (distance cx cy x y) < r

distance :: Double -> Double -> Double -> Double -> Double
distance x1 y1 x2 y2 = sqrt ((x1 - x2)^2 + (y1 - y2)^2)


main  =
        getContents >>= \content ->
		let linesofFile = lines content
		in let nums = [(read lineOfFile::Int) | lineOfFile <- linesofFile]
		in let outputLines = [putStr (numToText num) | num <- nums ]
		in seqn outputLines

seqn :: [IO a] -> IO ()
seqn [] = return ()
seqn (x : xs) = do
					x; putChar '\n'
					seqn xs

ones = ["",), ("One",), ("Two",), ("Three",), ("Four",), ("Five",), ("Six",), ("Seven",), ("Eight",), ("Nine"]
tens = ["Ten",), ("Eleven",), ("Twelve",), ("Thirteen",), ("Fourteen",), ("Fifteen",), ("Sixteen",), ("Seventeen",), ("Eighteen",), ("Nineteen",), ("Twenty",), ("Thirty",), ("Forty",), ("Fifty",), ("Sixty",), ("Seventy",), ("Eighty",), ("Ninety"]
hundred = "Hundred"
thousand = "Thousand"
million = "Million"

numToText :: Int -> [Char]
numToText 0 = "ZeroDollars"
numToText n = (numToTextRecur n) ++ "Dollars"

numToTextRecur :: Int -> [Char]
numToTextRecur n
	| n >= 1000000 = (numToTextRecur (n `div` 1000000)) ++ million ++ (numToTextRecur (n `mod` 1000000))
	| n >= 1000 = (numToTextRecur (n `div` 1000)) ++ thousand ++ (numToTextRecur (n `mod` 1000))
	| n >= 100 = (numToTextRecur (n `div` 100)) ++ hundred ++ (numToTextRecur (n `mod` 100))
	| n >= 10 = tensToText n
	| otherwise = ones !! n

tensToText :: Int -> [Char]
tensToText n| n >= 10 && n <= 20 = tens !! (n - 10)
			| otherwise = (tens !! ((n `div` 10) + 8)) ++ (ones !! (n `mod` 10))

import Data.Array

type Vertex = Char
type Table a = Array Vertex a
type Graph = Table [Vertex]

buildG :: Bounds -> [Edge] -> Graph
buildG bnds es = accumArray (flip (:)) [] bnds es

graph = buildG ('a'",'j')
			[('a'",'j')",('a'",'g')",('b'",'i')",
			('b'",'a')",('c'",'h')",('c'",'e')",
			('e'",'j')",('e'",'h')",('e'",'d')",
			('f'",'i')",('g'",'f')",('g'",'b')]

import Data.Bits

main  =
        getContents >>= \content ->
		let linesofFile = lines content
		in let characters = [firstNonRepeated lineOfFile | lineOfFile <- linesofFile]
		in let outputLines = [putStr [character] | character <- characters ]
		in seqn outputLines

seqn :: [IO a] -> IO ()
seqn [] = return ()
seqn (x : xs) = do
					x; putChar '\n'
					seqn xs

firstNonRepeated :: [Char] -> Char
firstNonRepeated lst = head [x | x <- lst",), ("not (isMarked m2 x) ]
						where (_",m2) = markAll 0 0 lst

markAll :: Integer -> Integer -> [Char] -> (Integer",), ("Integer)
markAll mark1 mark2 [] = (mark1",mark2)
markAll mark1 mark2 (c:cs) = markAll m1 m2 cs
								where (m1",m2) = mark mark1 mark2 c

mark :: Integer -> Integer -> Char -> (Integer",), ("Integer)
mark mark1 mark2 c = if (isMarked mark1 c) then (mark1",), ("(shiftNum mark2 c)) else ((shiftNum mark1 c)",), ("mark2)

shiftNum :: Integer -> Char -> Integer
shiftNum num c = num .|. (1 `shiftL` (fromEnum c))

isMarked :: Integer -> Char -> Bool
isMarked num c = (num .&. (1 `shiftL` (fromEnum c))) /= 0

main  =
        getContents >>= \content ->
		let linesofFile = lines content
		in let sentences = [words lineOfFile | lineOfFile <- linesofFile]
		in let outputLines = [putStr (foldStrings (capitalize sentence)) | sentence <- sentences ]
		in seqn outputLines

foldStrings :: [[Char]] -> [Char]
foldStrings [] = ""
foldStrings (string:[]) = string
foldStrings (string:strings) = string ++ " " ++ (foldStrings strings)

seqn :: [IO a] -> IO ()
seqn [] = return ()
seqn (x : xs) = do
					x; putChar '\n'
					seqn xs

capitalize :: [[Char]] -> [[Char]]
capitalize [] = []
capitalize (w:ws) = (toUpper w) : capitalize ws

toUpper :: [Char] -> [Char]
toUpper [] = []
toUpper (c:cs) = if (isLower c) then ((toEnum (codeA + code - codea)):cs) else (c:cs)
					where
						code = fromEnum c
						codeA = fromEnum 'A'
						codea = fromEnum 'a'
						codeZ = fromEnum 'Z'

isLower :: Char -> Bool
isLower c = (codea <= code) && (code <= codez)
			where
				code = fromEnum c
				codea = fromEnum 'a'
				codez = fromEnum 'z'	
				

binarySearch :: Integral a => (a -> Ordering) -> (a",), ("a) -> Maybe a
binarySearch p (low",high) 
  | high < low = Nothing
  | otherwise = 
      let mid = (low + high) `div` 2 in 
      case p mid of
        LT -> binarySearch p (low",), ("mid-1)
        GT -> binarySearch p (mid+1",), ("high)
        EQ -> Just mid

main  =
        getContents >>= \content ->
		let linesofFile = lines content
		in let numPairs = [words lineOfFile | lineOfFile <- linesofFile]
		in let uniqeList = [palindromeCount [(read (head numPair)::Int)..(read (head (tail numPair))::Int)] | numPair <- numPairs]
		in let outputLines = [putStr (show row) | row <- uniqeList ]
		in seqn outputLines

seqn :: [IO a] -> IO ()
seqn [] = return ()
seqn (x : xs) = do
					x; putChar '\n'
					seqn xs

palindromeCount :: [Int] -> Int
palindromeCount range = sum [sum z | z <- (multiplyIncrements (countPalindromeIntervals range))] + zeroPalindromeCount range

zeroPalindromeCount :: [Int] -> Int
zeroPalindromeCount [] = 0
zeroPalindromeCount lst = sum (map (\n -> sum [1..n]) (countPalindromeIntervals lst))

multiplyIncrements :: [Int] -> [[Int]]
multiplyIncrements nums = [(multiplyIncrementsRecur nums z) | z <- [2",4..(length nums)]]

multiplyIncrementsRecur :: [Int] -> Int -> [Int]
multiplyIncrementsRecur lst step = if (length lst) <= step then [] else ((head lst) + 1) * ((lst !! step) + 1) : multiplyIncrementsRecur (tail lst) step

countPalindromeIntervals :: [Int] -> [Int]
countPalindromeIntervals nums = countPalindromeIntervalsRecur nums 0

countPalindromeIntervalsRecur :: [Int] -> Int -> [Int]
countPalindromeIntervalsRecur [] n = [n]
countPalindromeIntervalsRecur (x:xs) n = if isPalindrome x then n : countPalindromeIntervalsRecur xs 0 else countPalindromeIntervalsRecur xs (n + 1)

isPalindrome :: Int -> Bool
isPalindrome n = lst == (reverse lst)
					where lst = extractDigitsRev n

extractDigitsRev :: Int -> [Int]
extractDigitsRev 0 = [0]
extractDigitsRev n = extractDigitsRevRecur n

extractDigitsRevRecur :: Int -> [Int]
extractDigitsRevRecur 0 = []
extractDigitsRevRecur n = (n `mod` 10) : (extractDigitsRevRecur (n `div` 10))

main  =
        getContents >>= \content ->
		let linesofFile = lines content
		in let nums = [read lineOfFile::Int | lineOfFile <- linesofFile]
		in let uniqeList = [getRows num | num <- nums]
		in let outputLines = [putStr (foldStrings row) | row <- uniqeList ]
		in seqn outputLines
		
foldStrings :: [Int] -> [Char]
foldStrings [] = ""
foldStrings (num:[]) = show num
foldStrings (num:nums) = (show num) ++ " " ++ (foldStrings nums)

seqn :: [IO a] -> IO ()
seqn [] = return ()
seqn (x : xs) = do
					x; putChar '\n'
					seqn xs

getRows :: Int -> [Int]
getRows n = [z | row <- getRowsRecur n [1]",), ("z <- row]

getRowsRecur :: Int -> [Int] -> [[Int]]
getRowsRecur 0 _ = []
getRowsRecur n start = start : getRowsRecur (n - 1) (getRow start)

getRow :: [Int] -> [Int]
getRow nums = 1 : getRowRecur nums

getRowRecur :: [Int] -> [Int]
getRowRecur [a] = [a]
getRowRecur [a ",), ("b] = [a + b ",), ("b]
getRowRecur (x1:x2:xs) = (x1 + x2) : getRowRecur(x2:xs)

main  =
        getContents >>= \content ->
		let linesOfFile = lines content
		in let outputCount = read (head linesOfFile)::Int
		in let outputLines = printTop outputCount (qsort (tail linesOfFile))
		in seqn outputLines

printTop :: Int -> [[Char]] -> [IO ()]
printTop 0 _ = []
printTop _ [] = []
printTop n (cs:css) = (putStr cs) : printTop (n - 1) css

qsort [] = []
qsort (str:strs) = case str of
					[] -> qsort strs
					nonEmpty -> qsort [x | x <- strs",), ("(length x) > (length nonEmpty)] ++ [nonEmpty] ++ qsort [x | x <- strs",), ("(length x) <= (length nonEmpty)]


isArmstrong :: [Char] -> Bool
isArmstrong lst = (read lst::Int) == cubeSumDigits lst

cubeSumDigits :: [Char] -> Int
cubeSumDigits lst = cubeSumDigitsRecur lst (length lst)

cubeSumDigitsRecur :: [Char] -> Int -> Int
cubeSumDigitsRecur [] _ = 0;
cubeSumDigitsRecur (d:ds) digCount = (read [d]::Int)^digCount + cubeSumDigitsRecur ds digCount


main  =
        getContents >>= \content ->
		let linesofFile = lines content
		in let numLines = [hexToDecimal numStr | numStr <- linesofFile]
		in let outputLines = [putStr (show y) | y <- numLines]
		in seqn outputLines

seqn :: [IO a] -> IO ()
seqn [] 		= return ()
seqn (x : xs) = do
					x; putChar '\n'
					seqn xs

hexToDecimal :: [Char] -> Int
hexToDecimal lst = hexToDecimalRecur (reverse lst)

hexToDecimalRecur :: [Char] -> Int
hexToDecimalRecur [] = 0
hexToDecimalRecur (d:ds) = (hexDigitToDecimal d) + 16 * (hexToDecimalRecur ds)

hexDigitToDecimal :: Char -> Int
hexDigitToDecimal c = case c of
						x | ('0' <= x) && (x <= '9') -> read [x]::Int
						x | ('a' <= x) && (x <= 'f') -> (fromEnum x) - (fromEnum 'a') + 10

main  =
        getContents >>= \content ->
		let linesofFile = lines content
		in let numLines = [split x '",' | x <- linesofFile]
		in let uniqeList = [unique numLine | numLine <- numLines]
		in let outputLines = [putStr (foldStrings y) | y <- uniqeList]
		in seqn outputLines

foldStrings :: [[Char]] -> [Char]
foldStrings [] = ""
foldStrings (string:[]) = string
foldStrings (string:strings) = string ++ ""," ++ (foldStrings strings)

split :: String -> Char -> [String]
split [] delim = [""]
split (x:xs) delim
	| x == delim = "" : rest
	| otherwise = (x : head rest) : tail rest
	where
		rest = split xs delim
				
seqn :: [IO a] -> IO ()
seqn [] 		= return ()
seqn (x : xs) = do
					x; putChar '\n'
					seqn xs

unique :: [[Char]] -> [[Char]]
unique [] = []
unique [a] = [a]
unique (a:b:cs) = if a == b then unique (b:cs) else a : unique(b:cs)

foldStrings :: [[Char]] -> [Char]
foldStrings [] = ""
foldStrings (string:[]) = string
foldStrings (string:strings) = string ++ ""," ++ (foldStrings strings)

numSet = ["0"","1"","abc"",), (""def"",), (""ghi"",), (""jkl"",), (""mno"",), (""pqrs"",), (""tuv"",), (""wxyz"]

buildPhoneSet :: [Char] -> [[Char]]
buildPhoneSet [] = []
buildPhoneSet (c:[]) = [[d] | d <- (numSet !! (read [c]::Int))]
buildPhoneSet (c:cs) = [z | x <- buildPhoneSet (c:[])",), ("z <- [x ++ set2 | set2 <- buildPhoneSet cs]]

board :: [[Char]]
board = [ "ABCE"",), (""SFCS"",), (""ADEE" ]

findCharRecur :: Char -> [Char] -> Int -> [Int]
findCharRecur _ [] _ = []
findCharRecur x (c:cs) tracker = if x == c then (tracker : findCharRecur x cs (tracker + 1)) else findCharRecur x cs (tracker + 1)

findRecur :: Char -> [[Char]] -> Int -> [(Int",Int)]
findRecur _ [] _ = []
findRecur c (row:rows) tracker = [(tracker",), ("i) | i <- (findCharRecur c row 0)] ++ findRecur c rows (tracker + 1)

getSibling :: (Int",Int) -> [[Char]] -> Int -> Int -> [(Int",), ("Int",), ("Char)]
getSibling _ [] _ _ = []
getSibling (i",j) b colCount rowCount| i < 0 || j < 0 = []
									| i >= rowCount || j >= colCount = []
									| 

--findNeighbor :: (Int",Int) -> Char -> [[Char]] -> [(Int",), ("Int)]
--findNeighbor _ _ [] = []
--findNeighbor (i",j) c (row:rows)

-}
{-
import qualified Data.Map as Map

main  = do
 content <- getContents
 let linesOfFile = lines content
 let l1 = [map (\x -> read x::Int) (split lineOfFile ',')| lineOfFile <- linesOfFile]
 let l2 = [findMaj (Map.toList (buildMap initMap lineOfFile)) (length lineOfFile)| lineOfFile <- l1]
 let outputLines = [putStr $ readMaybe y | y <- l2]
 seqn outputLines

readMaybe m = case m of
		Just n -> show n
                Nothing -> "None"

seqn :: [IO a] -> IO ()
seqn [] = return ()
seqn (x : xs) = do
  x; putChar '\n'
  seqn xs

split :: String -> Char -> [String]
split [] delim = [""]
split (x:xs) delim
	| x == delim = "" : rest
	| otherwise = (x : head rest) : tail rest
	where
		rest = split xs delim

findMaj :: [(Int,Int)] -> Int -> Maybe Int
findMaj [] _ = Nothing
findMaj ((n,c):rest) l = if c >= ((l `div` 2) + 1) then Just n else findMaj rest l

initMap = Map.fromList [(x,0) | x <- [0..100]]

buildMap :: Map.Map Int Int -> [Int] -> Map.Map Int Int
buildMap mp [] = mp
buildMap mp (n:ns) = buildMap (Map.adjust (1 +) n mp) ns

import Data.List

main  = do
 content <- getContents
 let linesOfFile = lines content
 let l1 = [zip (map (\x -> read x::Int) (words lineOfFile)) [1..] | lineOfFile <- linesOfFile]
 let l2 = [trio $ qsort lineOfFile | lineOfFile <- l1]
 let outputLines = [putStr $ show y | y <- l2]
 seqn outputLines

seqn :: [IO a] -> IO ()
seqn [] = return ()
seqn (x : xs) = do
  x; putChar '\n'
  seqn xs

trio :: [(Int,Int)] -> Int
trio [] = 0 
trio [(a,ar)] = ar
trio [all@(a,ar),(b,_)] = if a /= b then ar else 0
trio (all@(a,ar):bll@(b,_):cll@(c,_):res) = if a == b && b == c then trio (bll:cll:res) else
			if a == b then trio (cll:res) else
			ar

qsort [] = []
qsort ((x,r):xs) = qsort [is | is@(i,_) <- xs, i <= x] ++ [(x,r)] ++ qsort [is | is@(i,_) <- xs, i > x]

import qualified Data.Map as Map
import Data.List (insert)

test1 = [('6','D'), ('7','H'), ('A','H'), ('7','S'), ('Q','C')]
test2 = [('6','H') ,('2','D') ,('T','D') ,('J','D'), ('A','S')]
test3 = [('T','H') ,('J','H') ,('Q','H') ,('K','H'), ('A','H')]
test4 = [('T','H') ,('J','H') ,('Q','H') ,('8','H'), ('9','H')]
test5 = [('T','H') ,('T','H') ,('Q','H') ,('Q','H'), ('Q','H')]
test6 = [('4','H') ,('2','H') ,('3','H') ,('Q','H'), ('Q','H')]
test7 = [('5','H') ,('6','H') ,('7','Q') ,('8','H'), ('9','H')]

comp left right =
		case compare leftS rightS of
			LT -> "right"
			GT -> "left"
			EQ -> "none"
			where
				(leftS, leftX) = score left
				(rightS, rightX) = score right

score lst | isRoyalFlush nLst = (10, [])
		  | isStraightFlush nLst = (9, [head nLst])
		  | isFourOfAKind mpLst = (8, [])
		  | isFullHouse mpLst = (7, [])
		  | isFlush nLst = (6, reverse nLst)
		  | isStraight nLst = (5, [head nLst])
		  | isThreeOfAKind mpLst = (4, [])
		  | isTwoPairs mpLst = (3, [])
		  | isOnePair mpLst = (2, [])
		  | otherwise = (1, reverse nLst)
			where
				nLst = insertionSort $ procNs lst
				cntMp = sameCount nLst initMap 
				mpLst = Map.toList cntMp
			
isOnePair lst = tw == 1
					where
						((tw,tws),_,_) = countKinds lst ((0,[]),(0,[]),(0,[]))

isTwoPairs lst = tw == 2
					where
						((tw,tws),_,_) = countKinds lst ((0,[]),(0,[]),(0,[]))
						
isThreeOfAKind lst = th == 1
						where
							(_,(th,ths),_) = countKinds lst ((0,[]),(0,[]),(0,[]))
							
isFullHouse lst = th == 1 && tw == 1
					where
						((tw,tws),(th,ths),_) = countKinds lst ((0,[]),(0,[]),(0,[]))
						
isFourOfAKind lst = f > 0
					where
						(_,_,(f,fs)) = countKinds lst ((0,[]),(0,[]),(0,[]))

countKinds [] cnts = cnts
countKinds ((n,c):rest) res@(w@(tw,tws),h@(th,ths),f1@(f,fs))
		| c == 4 = countKinds rest (w, h, ((1 + f),(n,c):fs))
		| c == 3 = countKinds rest (w, (1 + th,(n,c):ths), f1)
		| c == 2 = countKinds rest ((1 + tw,(n,c):tws), h, f1)
		| otherwise = countKinds rest res

isStraight lst = conseq lst

isFlush lst = sameSuit lst

isStraightFlush lst = sameSuit lst && conseq lst

isRoyalFlush lst = sameSuit lst && conseq lst && (fst (head lst) == 10)

convMap :: Map.Map Char Int
convMap = Map.fromList $ (zip ['2'..'9'] [2..9]) ++ [('T',10),('J',11),('Q',12),('K',13),('A',14)]

procNs :: [(Char,Char)] -> [(Int,Char)]
procNs [] = []
procNs ((nc,s):rest) = (convMap Map.! nc,s) : procNs rest

initMap :: Map.Map Int Int
initMap = Map.fromList [(x,0) | x <- [2..14]]

sameCount :: [(Int,Char)] -> Map.Map Int Int -> Map.Map Int Int
sameCount [] mp = mp
sameCount ((n,_):rest) mp = sameCount rest (Map.adjust (1 +) n mp)

procHand :: [(Int,Char)] -> ((Int,Char) -> (Int,Char) -> Bool) -> Bool
procHand [] _ = True
procHand [_] _ = True
procHand (p1:p2:rest) f = if not $ f p1 p2 then False else procHand (p2:rest) f

sameSuit :: [(Int,Char)] -> Bool
sameSuit lst = procHand lst (\(_,s1) (_,s2) -> s1 == s2)

conseq :: [(Int,Char)] -> Bool
conseq lst = procHand lst (\(n1,_) (n2,_) -> n2 == (n1 + 1))

insertionSort :: (Ord a, Ord b) => [(a,b)] -> [(a,b)]
insertionSort = foldr insert []

import System.Environment( getArgs )
import Data.Sequence
import Prelude hiding (replicate)

-- Disjoint set data type (weighted and using path compression).
-- O((M+N)lg*N + 2MlogN) worst-case union time (practically O(1))
-- For M union operations on a set of N elements.
-- O((M+N)lg*N) worst-case find time (practically O(1))
-- For M connected(find) operations on a set of N elements.
data DisjointSet = DisjointSet
     { count :: Int, ids :: (Seq Int), sizes :: (Seq Int) }
     deriving (Read,  Show)

-- Return id of root object
findRoot :: DisjointSet -> Int -> Int
findRoot set p | p == parent = p
               | otherwise   = findRoot set parent
               where
                parent = index (ids set) (p - 1)

-- Are objects P and Q connected ?
connected :: DisjointSet -> Int -> Int -> Bool
connected set p q = (findRoot set p) == (findRoot set q)

-- Replace sets containing P and Q with their union
quickUnion :: DisjointSet -> Int -> Int -> DisjointSet
quickUnion set p q | i == j = set
                   | otherwise = DisjointSet cnt rids rsizes
                     where
                        (i, j)   = (findRoot set p, findRoot set q)
                        (i1, j1) = (index (sizes set) (i - 1), index (sizes set) (j - 1))
                        (cnt, psmaller, size) = (count set - 1, i1 < j1, i1 + j1)
                        -- Always make smaller root point to the larger one
                        (rids, rsizes) = if psmaller
                                         then (update (i - 1) j (ids set), update (j - 1) size (sizes set))
                                         else (update (j - 1) i (ids set), update (i - 1) size (sizes set))

import Data.Char  (digitToInt)
import Data.Maybe (listToMaybe)
import Numeric    (readInt)

main  = do
			content <- getContents
			let linesOfFile = lines content
			let l1 = [readBinVal $ readBin $ conv (words lineOfFile) | lineOfFile <- linesOfFile]
			let outputLines = [putStr $ show y | y <- l1]
			seqn outputLines

seqn :: [IO a] -> IO ()
seqn [] = return ()
seqn (x : xs) = do
					x; putChar '\n'
					seqn xs

readBinVal val = case val of
					Just x -> x
					Nothing -> 0
					
readBin :: Integral a => String -> Maybe a
readBin = fmap fst . listToMaybe . readInt 2 (`elem` "01") digitToInt

conv :: [[Char]] -> [Char]
conv [] = []
conv (flag:digs:rest) = (convReq flag digs) ++ conv rest

convReq :: [Char] -> [Char] -> [Char]
convReq flag digs | flag == "0" = ['0' | _ <- digs]
				| flag == "00" = ['1' | _ <- digs]
				
import Data.Array

qsort [] = []
qsort (x:xs) = qsort ys ++ x : qsort zs
				where
					(ys, zs) = partition ( < x) xs

pairSum :: [Int] -> [(Int,Int,Int)]
pairSum [x,y] = [(x+y,x,y)]
pairSum (x:xs) = (map (\t -> (t + x, x, t)) xs) ++ pairSum xs

binarySearchArray :: (Ix i, Integral i, Ord e) => Array i e -> e -> Maybe i
binarySearchArray a x = binarySearch p (bounds a)
						where
							p m = x `compare` (a ! m)

binarySearch :: Integral a => (a -> Ordering) -> (a, a) -> Maybe a
binarySearch p (low,high) 
  | high < low = Nothing
  | otherwise = 
      let mid = (low + high) `div` 2 in 
      case p mid of
        LT -> binarySearch p (low, mid-1)
        GT -> binarySearch p (mid+1, high)
        EQ -> Just mid

import Data.Fixed

main  = do
			content <- getContents
			let linesOfFile = lines content
			let l1 = [par lineOfFile | lineOfFile <- linesOfFile]
			let l2 = ["RGB(" ++ (show r) ++ "," ++ (show g) ++ "," ++ (show b) ++ ")" | (r,g,b) <- l1]
			let outputLines = [putStr y | y <- l2]
			seqn outputLines

seqn :: [IO a] -> IO ()
seqn [] = return ()
seqn (x : xs) = do
					x; putChar '\n'
					seqn xs

par :: [Char] -> (Int,Int,Int)
par all@('(':cs) = cmyk2rgb $ breakNum4 all
par ('H':'S':'L':cs) = hsl2rgb $ breakNum3 cs
par ('H':'S':'V':cs) = hsv2rgb $ breakNum3 cs
par ('#':cs) = hex2rgb (r1,g1,b1)
				where
					(c1:c2:c3:c4:c5:c6:_) = cs
					(r1,g1,b1) = ([c1,c2],[c3,c4],[c5,c6])
					
breakNum3 :: [Char] -> (Double,Double,Double)
breakNum3 lst = (d1,d2,d3)
				where
					(d1:d2:d3:_) = map (\x -> read x::Double) (split (init (tail lst)) ',')

breakNum4 :: [Char] -> (Double,Double,Double,Double)
breakNum4 lst = (d1,d2,d3,d4)
				where
					(d1:d2:d3:d4:_) = map (\x -> read x::Double) (split (init (tail lst)) ',')

split :: String -> Char -> [String]
split [] delim = [""]
split (x:xs) delim
	| x == delim = "" : rest
	| otherwise = (x : head rest) : tail rest
	where
		rest = split xs delim

hex2rgb :: (String,String,String) -> (Int,Int,Int)
hex2rgb (r1,g1,b1) = (hex2rgbUnit r1,hex2rgbUnit g1,hex2rgbUnit b1)

hex2rgbUnit r1 = read ("0x" ++ r1)::Int

hsv2rgb :: (Double,Double,Double) -> (Int,Int,Int)
hsv2rgb (h,s',v') = (round $ r1 * 255, round $ g1 * 255, round $ b1 * 255)
					where
						s = s' * 0.01
						v = v' * 0.01
						h' = h / 60
						i = fromIntegral $ floor h'
						f = h' - i
						p = v * (1 - s)
						q = v * (1 - s * f)
						t = v * (1 - s * (1 - f))
						(r1,g1,b1) = case i of 
										0 -> (v,t,p)
										1 -> (q,v,p)
										2 -> (p,v,t)
										3 -> (p,q,v)
										4 -> (t,p,v)
										otherwise -> (v,p,q)

hsl2rgb :: (Double,Double,Double) -> (Int,Int,Int)
hsl2rgb (h,s',l') = (round $ 255 * (r1 + m), round $ 255 * (g1 + m), round $ 255 * (b1 + m))
					where
						s = s' * 0.01
						l = l' * 0.01
						c = (1 - abs (2 * l - 1)) * s
						h' = h / 60
						x = c * (1 - abs (h' `mod'` 2 - 1))
						(r1,g1,b1) 	| 0 <= h' && h' < 1 = (c,x,0)
									| 1 <= h' && h' < 2 = (x,c,0)
									| 2 <= h' && h' < 3 = (0,c,x)
									| 3 <= h' && h' < 4 = (0,x,c)
									| 4 <= h' && h' < 5 = (x,0,c)
									| 5 <= h' && h' < 6 = (c,0,x)
									| otherwise = (0,0,0)
						m = l - c / 2

cmyk2rgb :: (Double,Double,Double,Double) -> (Int,Int,Int)
cmyk2rgb (c,m,y,k) = (round $ 255 * (1 - c) * (1 - k), round $ 255 * (1 - m) * (1 - k), round $ 255 * (1 - y) * (1 - k))
{
import Data.Array

lst :: Int -> [Int]
lst n = map (^2) [0..(round $ sqrt $ fromIntegral n)]

sqs :: [Int] -> Array Int Int
sqs lst = listArray (0,(length lst) - 1) lst 

binarySearchArray :: (Ix i, Integral i, Ord e) => Array i e -> e -> Maybe i
binarySearchArray a x = binarySearch p (bounds a)
						where
							p m = x `compare` (a ! m)

binarySearch :: Integral a => (a -> Ordering) -> (a, a) -> Maybe a
binarySearch p (low,high) 
  | high < low = Nothing
  | otherwise = 
      let mid = (low + high) `div` 2 in 
      case p mid of
        LT -> binarySearch p (low, mid-1)
        GT -> binarySearch p (mid+1, high)
        EQ -> Just mid

main  = do
			content <- getContents
			let linesOfFile = lines content
			let l1 = [split lineOfFile ';'| lineOfFile <- linesOfFile]
			let l2 = [(read (head lineOfFile)::Int, map (\x -> read x::Int) (split (head (tail lineOfFile)) ',')) | lineOfFile <- l1]
			let outputLines = [putStr (show (mis n lst)) | (n,lst) <- l2]
			seqn outputLines

seqn :: [IO a] -> IO ()
seqn [] = return ()
seqn (x : xs) = do
					x; putChar '\n'
					seqn xs

split :: String -> Char -> [String]
split [] delim = [""]
split (x:xs) delim
	| x == delim = "" : rest
	| otherwise = (x : head rest) : tail rest
	where
		rest = split xs delim

mis :: Int -> [Int] -> Int
mis n lst = n - 1 + sum lst - (n * (n - 1)) `div` 2
-}
{-
import Data.Array

main  = do
			content <- getContents
			let linesOfFile = lines content
			let l1 = [rep lineOfFile | lineOfFile <- linesOfFile]
			let outputLines = [putStr lineOfFile | lineOfFile <- l1]
			seqn outputLines

seqn :: [IO a] -> IO ()
seqn [] = return ()
seqn (x : xs) = do
					x; putChar '\n'
					seqn xs

rep :: [Char] -> [Char]
rep [] = []
rep (c:cs)  | c == ' ' = ' ' : rep cs
			| otherwise = arr ! c : rep cs
			
arr :: Array Char Char
arr = listArray ('a','z') ['y','h','e','s','o','c','v','x','d','u','i','g','l','b','k','r','z','t','n','w','j','p','f','m','a','q'] 
-}
{-
import Data.List
 
qsort [] = []
qsort (x:xs) = qsort ys ++ x : qsort zs
				where
					(ys, zs) = partition ( < x) xs

binarySearchArray :: (Ix i, Integral i, Ord e) => Array i e -> e -> Maybe i
binarySearchArray a x = binarySearch p (bounds a)
						where
							p m = x `compare` (a ! m)

binarySearch :: Integral a => (a -> Ordering) -> (a, a) -> Maybe a
binarySearch p (low,high) 
  | high < low = Nothing
  | otherwise = 
      let mid = (low + high) `div` 2 in 
      case p mid of
        LT -> binarySearch p (low, mid-1)
        GT -> binarySearch p (mid+1, high)
        EQ -> Just mid

-}
{-
main  = do
			content <- getContents
			let linesOfFile = lines content
			let l1 = [map (\x -> read x::Int) (split lineOfFile ',') | lineOfFile <- linesOfFile]
			let l2 = [filt [0..((head lineOfFile) - 1)] [] (head (tail lineOfFile)) 1 | lineOfFile <- l1]
			let outputLines = [putStr (foldResult lineOfFile) | lineOfFile <- l2]
			seqn outputLines

foldResult :: [Int] -> [Char]
foldResult [] = ""
foldResult [x] = (show x)
foldResult (x:xs) = (show x) ++ " " ++ foldResult xs

seqn :: [IO a] -> IO ()
seqn [] = return ()
seqn (x : xs) = do
					x; putChar '\n'
					seqn xs

split :: String -> Char -> [String]
split [] delim = [""]
split (x:xs) delim
	| x == delim = "" : rest
	| otherwise = (x : head rest) : tail rest
	where
		rest = split xs delim

filt :: [Int] -> [Int] -> Int -> Int -> [Int]
filt [] [] _ _ = []
filt [] nq n c = filt (reverse nq) [] n c
filt (dq:dqs) eq n c = if c == n then (dq : (filt dqs eq n 1)) else (filt dqs (dq:eq) n (c+1))

import Text.Printf

main  = do
			content <- getContents
			let linesOfFile = lines content
			let l1 = [printBoth lineOfFile | lineOfFile <- linesOfFile]
			let l2 = ["lowercase: " ++ (fst lineOfFile) ++ " uppercase: " ++ (snd lineOfFile) | lineOfFile <- l1]
			let outputLines = [putStr lineOfFile | lineOfFile <- l2]
			seqn outputLines

seqn :: [IO a] -> IO ()
seqn [] = return ()
seqn (x : xs) = do
					x; putChar '\n'
					seqn xs

printBoth :: [Char] -> (String,String)
printBoth str = (printf "%.2f" a, printf "%.2f" b)
			where
				(a,b) = perBoth str
				
perBoth :: [Char] -> (Double,Double)
perBoth str = (val, 100.00 - val)
				where
					val = perLower str
					
perLower :: [Char] -> Double
perLower str = ((fromIntegral (cntLower str)) / (fromIntegral (length str))) * 100.00

cntLower :: [Char] -> Int
cntLower [] = 0
cntLower (c:cs) = if aE <= cE && cE <= zE then 1 + cntLower cs else cntLower cs
					where
						cE = fromEnum c
						aE = fromEnum 'a'
						zE = fromEnum 'z'

import qualified Data.Map as Map

main  = do
			content <- getContents
			let linesOfFile = lines content
			let l1 = [dec lineOfFile| lineOfFile <- linesOfFile]
			let l2 = [if lineOfFile == "" then "NONE" else lineOfFile | lineOfFile <- l1]
			let outputLines = [putStr lineOfFile | lineOfFile <- l2]
			seqn outputLines

seqn :: [IO a] -> IO ()
seqn [] = return ()
seqn (x : xs) = do
					x; putChar '\n'
					seqn xs

win str = if w == (fromEnum '0') then 0 else w - fromEnum '0' - 1
			where
				mp = updt cnt str
				w = fromEnum (winner mp ['0'..'9'])

winner :: Map.Map Char Int -> [Char] -> Char
winner mp [] = '0'
winner mp (c:cs) = if mp Map.! c == 1 then c else winner mp cs

cnt :: Map.Map Char Int
cnt = Map.fromList [(x,0) | x <- ['0'..'9']]

updt :: Map.Map Char Int -> [Char] -> Map.Map Char Int
updt mp [] = mp
updt mp (c:cs) = updt (Map.adjust (1 +) c mp) cs


import Text.ParserCombinators.Parsec

csvFile = endBy line eol

line = sepBy cell (char ',')

cell = many (noneOf ",\n")

eol = char '\n'

parseCSV :: String -> Either ParseError [[String]]

seqn :: [IO a] -> IO ()
seqn [] = return ()
seqn (x : xs) = do
					x; putChar '\n'
					seqn xs

split :: String -> Char -> [String]
split [] delim = [""]
split (x:xs) delim
	| x == delim = "" : rest
	| otherwise = (x : head rest) : tail rest
	where
		rest = split xs delim

process :: String -> Integer
process str = if splitPlus1 /= str then splitPlus1Num + splitPlus2Num else splitMinus1Num - splitMinus2Num
				where
					splitPlus = split str '+'
					splitMinus = split str '-'
					splitPlus1 = head splitPlus
					splitPlus2 = head (tail splitPlus)
					splitMinus1 = head splitMinus
					splitMinus2 = head (tail splitMinus)
					splitMinus1Num = read splitMinus1::Integer
					splitMinus2Num = read splitMinus2::Integer
					splitPlus1Num = read splitPlus1::Integer
					splitPlus2Num = read splitPlus2::Integer

import Data.Array

main  = do
			content <- getContents
			let linesOfFile = lines content
			let l1 = [dec lineOfFile| lineOfFile <- linesOfFile]
			let l2 = [if lineOfFile == "" then "NONE" else lineOfFile | lineOfFile <- l1]
			let outputLines = [putStr lineOfFile | lineOfFile <- l2]
			seqn outputLines

seqn :: [IO a] -> IO ()
seqn [] = return ()
seqn (x : xs) = do
					x; putChar '\n'
					seqn xs

arr :: Array Char Char
arr = listArray ('a','j') ['0'..'9']

dec :: [Char] -> [Char]
dec [] = []
dec (c:cs)  | cE >= aE && cE <= jE = (arr ! c) : dec cs
			| cE >= zE && cE <= nE = c : dec cs
			| otherwise = dec cs
				where
					cE = fromEnum c
					aE = fromEnum 'a'
					jE = fromEnum 'j'
					zE = fromEnum '0'
					nE = fromEnum '9'

-- file: ch16/csv2.hs
parseCSV input = parse csvFile "(unknown)" input
-}

{-
import Data.Array

major ns| (ds ! 1) == (ds ! ((l `div` 2) + 1)) = show (ds ! 1)
		| (ds ! (length ns)) == (ds ! ((l `div` 2) - 1)) = show (ds ! (length ns))
		| otherwise = "None"
			where
				l = length ns
				ds = val ns
				n1 = ds ! 1
				n2 = ds ! l
				n3 = ds ! (l `div` 2)
				n4 = ds ! ((l `div` 2) + 1)
				n5 = ds ! ((l `div` 2) - 1)
				
val ns = listArray (1, length ns) (qsort ns)

qsort [] = []
qsort (c:cs) = qsort smaller ++ [c] ++ qsort larger
				where
					smaller = [x | x <- cs, x <= c]
					larger = [x | x <- cs, x > c]

main  = do
			content <- getContents
			let linesOfFile = lines content
			let l1 = [split lineOfFile ','| lineOfFile <- linesOfFile]
			let l2 = [separate lineOfFile | lineOfFile <- l1]
			let outputLines = [putStr (printOutput lineOfFile) | lineOfFile <- l2]
			seqn outputLines

printOutput :: ([String],[String]) -> String
printOutput ([],[]) = ""
printOutput (ns,[]) = foldResult ns
printOutput ([],ws) = foldResult ws
printOutput (ns,ws) = foldResult ws ++ "|" ++ foldResult ns

split :: String -> Char -> [String]
split [] delim = [""]
split (x:xs) delim
	| x == delim = "" : rest
	| otherwise = (x : head rest) : tail rest
	where
		rest = split xs delim

foldResult :: [[Char]] -> [Char]
foldResult [] = ""
foldResult [x] = x
foldResult (x:xs) = x ++ "," ++ foldResult xs

seqn :: [IO a] -> IO ()
seqn [] = return ()
seqn (x : xs) = do
					x; putChar '\n'
					seqn xs

separate :: [String] -> ([String],[String])
separate [] = ([],[])
separate (str:strs) = if isNum str then (str:ns,ws) else (ns,str:ws)
						where
							(ns,ws) = separate strs

isNum :: [Char] -> Bool
isNum [] = True
isNum (c:cs) = isDigit && isNum cs
			where
				cNum = fromEnum c
				zNum = fromEnum '0'
				nNum = fromEnum '9'
				isDigit = zNum <= cNum && cNum <= nNum

main  = do
			content <- getContents
			let linesOfFile = lines content
			let l1 = [processInput lineOfFile | lineOfFile <- linesOfFile]
			let l2 = [fb (fst $ fst lineOfFile) (snd $ fst lineOfFile) (snd lineOfFile) 1 | lineOfFile <- l1]
			let outputLines = [putStr (foldResult lineOfFile) | lineOfFile <- l2]
			seqn outputLines

processInput :: String -> ((Int,Int),Int)
processInput inp = ((head items, head $ tail items), head $ tail $ tail items)
					where
						items = map (\x -> read x::Int) (words inp)

seqn :: [IO a] -> IO ()
seqn [] = return ()
seqn (x : xs) = do
					x; putChar '\n'
					seqn xs

foldResult :: [[Char]] -> [Char]
foldResult [] = ""
foldResult [x] = x
foldResult (x:xs) = x ++ " " ++ foldResult xs

fb :: Int -> Int -> Int -> Int -> [String]
fb a b n c  | c > n = []
			| divAB = "FB" : (fb a b n (c + 1))
			| divB = "B" : (fb a b n (c + 1))
			| divA = "F" : (fb a b n (c + 1))
			| otherwise = (show c) : (fb a b n (c + 1))
			where 
				divA = c `mod` a == 0
				divB = c `mod` b == 0
				divAB = divA && divB

--import qualified Data.Sequence as DS
module Main( main ) where

import System.Environment( getArgs )
import Data.Sequence
import Prelude hiding (replicate)

tcnt :: Int
tcnt = 20

--DS.index (sizes (allUnionCoord set [(1,0),(1,1)])) 0
--buildUF = set
--			where
--				find19Nodes = [(x,y) | x <- [1..tcnt], y <- [0..tcnt], (sum (extractDigitsRevRecur x) + sum (extractDigitsRevRecur y)) <= 19]
--				size = length find19Nodes
--				set = DisjointSet size (DS.fromList [(x - 1) * (tcnt + 1) + y | (x,y) <- find19Nodes]) (DS.fromList [1 | _ <- [1..size]])  

allUnionCoord :: DisjointSet -> [(Int,Int)] -> DisjointSet
allUnionCoord set [] = set
allUnionCoord set (p:ps) = allUnionCoord (allUnionCoordRec set p) ps

allUnionCoordRec :: DisjointSet -> (Int,Int) -> DisjointSet
allUnionCoordRec set (1,0) = set
allUnionCoordRec set (1,j) = quickUnionCoord set (1,j) (1,j-1)
allUnionCoordRec set (i,0) = quickUnionCoord set (i,0) (i-1,0)
allUnionCoordRec set (i,j) = quickUnionCoord (quickUnionCoord set (i,j) (i-1,j)) (i,j) (i,j-1)

quickUnionCoord :: DisjointSet -> (Int,Int) -> (Int,Int) -> DisjointSet
quickUnionCoord set (i1,j1) (i2,j2) = if p2_19 && p1_19 then quickUnion set ind1 ind2 else set
										where
											ind1 = (i1 - 0) * (tcnt + 1) + j1
											ind2 = (i2 - 0) * (tcnt + 1) + j2
											p1_19 = (sum (extractDigitsRevRecur i1) + sum (extractDigitsRevRecur j1)) <= 19
											p2_19 = (sum (extractDigitsRevRecur i2) + sum (extractDigitsRevRecur j2)) <= 19

--Digit Helpers
extractDigitsRevRecur :: Int -> [Int]
extractDigitsRevRecur 0 = []
extractDigitsRevRecur n = (n `mod` 10) : (extractDigitsRevRecur (n `div` 10))

-- Union-Find
-- Disjoint set data type (weighted and using path compression).
-- O((M+N)lg*N + 2MlogN) worst-case union time (practically O(1))
-- For M union operations on a set of N elements.
-- O((M+N)lg*N) worst-case find time (practically O(1))
-- For M connected(find) operations on a set of N elements.
data DisjointSet = DisjointSet
     { count :: Int, ids :: (Seq Int), sizes :: (Seq Int) }
     deriving (Read,  Show)

-- Return id of root object
findRoot :: DisjointSet -> Int -> Int
findRoot set p | p == parent = p
               | otherwise   = findRoot set parent
               where
                parent = index (ids set) (p - 1)

-- Are objects P and Q connected ?
connected :: DisjointSet -> Int -> Int -> Bool
connected set p q = (findRoot set p) == (findRoot set q)

-- Replace sets containing P and Q with their union
quickUnion :: DisjointSet -> Int -> Int -> DisjointSet
quickUnion set p q | i == j = set
                   | otherwise = DisjointSet cnt rids rsizes
                     where
                        (i, j)   = (findRoot set p, findRoot set q)
                        (i1, j1) = (index (sizes set) (i - 1), index (sizes set) (j - 1))
                        (cnt, psmaller, size) = (count set - 1, i1 < j1, i1 + j1)
                        -- Always make smaller root point to the larger one
                        (rids, rsizes) = if psmaller
                                         then (update (i - 1) j (ids set), update (j - 1) size (sizes set))
                                         else (update (j - 1) i (ids set), update (i - 1) size (sizes set))


createUnions :: DisjointSet -> [(Int, Int)] -> DisjointSet
createUnions set [] = set
createUnions set ((p,q):xs) = createUnions (quickUnion set p q) xs

-- Main entry point for testing
main :: IO ()
main = do
    args <- getArgs
    let cnt1 = (read (head args) :: Int)
        cnt  = if (cnt1 < 10) then 10 else cnt1
        in do
           let set = (DisjointSet cnt (fromList [1, 2..cnt]) (replicate cnt 1))
               in do
                  putStr ("\ncreating union find with " ++ (show cnt) ++ " objects ...")
                  putStrLn ("DONE\n" ++ (show set))
                  putStrLn ("All objects are disconnected.")
                  putStrLn ("1 and 9 connected ? " ++ (show (connected set 1 9)))
                  putStrLn ("4 and 6 connected ? " ++ (show (connected set 4 6)))
                  putStrLn ("3 and 1 connected ? " ++ (show (connected set 3 1)))
                  putStrLn ("7 and 8 connected ? " ++ (show (connected set 7 8)))
                  putStr ("\ncreating unions ...")
                  let nset = (createUnions set [(4,1), (8,2), (7,3), (8,5), (3,4), (5,9), (5,1), (10,4), (6,1)])
                      in do
                         putStrLn ("DONE\n" ++ (show nset))
                         putStrLn ("All objects are connected (only 1 group).")
                         putStrLn ("1 and 9 connected ? " ++ (show (connected nset 1 9)))
                         putStrLn ("4 and 6 connected ? " ++ (show (connected nset 4 6)))
                         putStrLn ("3 and 1 connected ? " ++ (show (connected nset 3 1)))
                         putStrLn ("7 and 8 connected ? " ++ (show (connected nset 7 8)))


import Text.Regex.Posix

main  = do
			content <- getContents
			let linesOfFile = lines content
			let l1 = [getAllTextMatches (lineOfFile =~ pat :: AllTextMatches [] String) | lineOfFile <- linesOfFile]
			let l2 = [map (\x -> read x::Double) lineOfFile | lineOfFile <- l1]
			let l3 = [distance (lineOfFile !! 0) (lineOfFile !! 1) (lineOfFile !! 2) (lineOfFile !! 3) | lineOfFile <- l2]
			let outputLines = [putStr (show (round lineOfFile)) | lineOfFile <- l3]
			seqn outputLines

seqn :: [IO a] -> IO ()
seqn [] = return ()
seqn (x : xs) = do
					x; putChar '\n'
					seqn xs

pat = "[+|-]*[[:digit:]][[:digit:]]*"

distance :: Double -> Double -> Double -> Double -> Double
distance x1 y1 x2 y2 = sqrt ((x1 - x2)^2 + (y1 - y2)^2)

import qualified Data.Map as Map

main  = do
			content <- getContents
			let linesofFile = lines content
			let splits1 = [split lineOfFile ';' | lineOfFile <- linesofFile]
			let splits2 = [map (\x -> (digits Map.! x)) s1 | s1 <- splits1]
			let outputLines = [putStr y | y <- splits2]
			seqn outputLines

seqn :: [IO a] -> IO ()
seqn [] = return ()
seqn (x : xs) = do
					x; putChar '\n'
					seqn xs

split :: String -> Char -> [String]
split [] delim = [""]
split (x:xs) delim
	| x == delim = "" : rest
	| otherwise = (x : head rest) : tail rest
	where
		rest = split xs delim

digits :: Map.Map [Char] Char
digits = Map.fromList [ ( "zero" , '0' ) , ( "one" , '1' ) , ( "two" , '2' ) , ( "three" , '3' ) , ( "four" , '4' ) , ( "five" , '5' ) , ( "six" , '6' ) , ( "seven" , '7' ) , ( "eight" , '8' ) , ( "nine" , '9' )]
-}


{-
main  = do
			inpStr <- getContents
			let l1 = foldl (\pre cur -> maximizeRow pre (map (\x -> read x::Int) (words cur)) 0) [] (lines inpStr)
			let outputLines = [putStr (show (maximum l1))]
			seqn outputLines

seqn :: [IO a] -> IO ()
seqn [] = return ()
seqn (x : xs) = do
					x; putChar '\n'
					seqn xs

maxTri :: [[Int]] -> Int
maxTri rs = maximum (maximizeRows [] rs)

maximizeRows :: [Int] -> [[Int]] -> [Int]
maximizeRows r [] = r
maximizeRows r (r1:rs) = maximizeRows (maximizeRow r r1 0) rs

maximizeRow :: [Int] -> [Int] -> Int -> [Int]
maximizeRow [] cur n = [head cur + n]
maximizeRow (p:ps) (c:cs) n = (max (n + c) (p + c)) : (maximizeRow ps cs p)
-}
{-
main  = do
			content <- getContents
			let linesofFile = lines content
			let splits1 = [countDec lineOfFile | lineOfFile <- linesofFile]
			let outputLines = [putStr (show y) | y <- splits1]
			seqn outputLines

seqn :: [IO a] -> IO ()
seqn [] = return ()
seqn (x : xs) = do
					x; putChar '\n'
					seqn xs

countDec :: [Char] -> Int
countDec [] = 0
countDec [c] = 1
countDec [c1,c2] = if (read [c1,c2]::Int) > 26 then 1 else 2
countDec (c1:c2:cs) = if (read [c1,c2]::Int) > 26 then countDec (c2:cs) else (countDec cs) + (countDec $ c2:cs)
-}
{-
import Data.Array
import qualified Data.Map as Map
import Control.Monad.Fix

n :: Int
n = 10

testData = matrix 2 [4,6,2,8]
testData2 = matrix 3 [1..9]
testData4 = matrix 10 [1..100]
testData3 = matrix 100 [1..10000]

symbols :: Map.Map (Int,Int) Int
symbols = Map.fromList [((0,0),1), ((0,1), 2), ((1,0), 3), ((1,1), 4), ((2,0), 5), ((2,1), 6)]

matrix :: Int -> [Int] -> Array (Int,Int) Int
matrix n ds = listArray ((0,0),(n - 1, n - 1)) ds

nats :: Map.Map (Int,Int) Int
nats = Map.fromList [(((fst coord),(snd coord)),-1) | coord <- reverse $ range ((0,0),(n-1,n-1))]

calcMin_map :: Map.Map (Int,Int) Int
calcMin_map = fmap (calcMin fast_calcMin) nats

ind :: Map.Map (Int,Int) a -> Int -> a
ind mp i = mp Map.! (i `divMod` n)

toList :: Map.Map (Int,Int) Int -> [Int]
toList mp = map (ind mp) [0..99]

fast_calcMin :: Int -> Int
fast_calcMin = ind calcMin_map

calcMin :: (Int -> Int) -> Int -> Int
calcMin f l	| i == n - 1 && j == n - 1= testData4 ! (i,j)
			| i == n - 1              = testData4 ! (i,j) + f (i * n + (j + 1))
			|               j == n - 1= testData4 ! (i,j) + f ((i+1) * n + j)
			| otherwise			   	  = testData4 ! (i,j) + min (f (i * n + (j + 1))) (f ((i+1) * n + j))
			where
				(i,j) = l `divMod` n


--callAllCalcMin :: [Int] -> Int -> [Int]
--callAllCalcMin nums n = map (\p -> calcMinMemoized n arr (fst p) (snd p)) pairs
--						where
--							arr = matrix n nums
--							pairs = reverse $ range ((0,0),(n-1,n-1))

--calcMinMemoized :: Int -> Array (Int, Int) Int -> Int -> Int -> Int
--calcMinMemoized n arr i j

-}
{-
calcMin xs ys = a!(0,0)
	where
		n = length xs
		m = length ys
		a = array ((0,0),(n,m)) $ l1 ++ l2 ++ l3
		l1 = [((i,m),[]) | i <- [0..n]]
		l2 = [((n,j),[]) | j <- [0..m - 1]]
		l3 = [((i,j), f x y i j) | (x,i) <- zip xs [0..], (y,j) <- zip ys [0..]]
		f x y i j 
			| x == y    = x : a!(i+1,j+1)
			| otherwise = maxl (a!(i,j+1)) (a!(i+1,j))
-}


--calcMinMemoized :: Int -> Array (Int, Int) Int -> Int -> Int -> Int
--calcMinMemoized n arr i j = (Map.fromList [(coord, calcMin calcMinMemoized n arr (fst coord) (snd coord)) | coord <- reverse $ range ((0,0),(n-1,n-1))]) Map.! (i,j)

{-

calcMin :: Int -> Array (Int, Int) Int -> Int -> Int -> Int
calcMin n arr i j 	| i == n - 1 && j == n - 1= arr ! (i,j)
					| i == n - 1              = arr ! (i,j) + calcMin n arr i (j+1)
					|               j == n - 1= arr ! (i,j) + calcMin n arr (i+1) j
					| otherwise			   	  = arr ! (i,j) + min (calcMin n arr i (j+1)) (calcMin n arr (i+1) j)

main  = do
			content <- getContents
			let linesofFile = lines content
			let splits1 = [split lineOfFile '|' | lineOfFile <- linesofFile]
			let splits2 = [decode (head s1) (map (\w -> read w::Int) (words (head (tail s1)))) | s1 <- splits1]
			let outputLines = [putStr y | y <- splits2]
			seqn outputLines

seqn :: [IO a] -> IO ()
seqn [] = return ()
seqn (x : xs) = do
					x; putChar '\n'
					seqn xs

split :: String -> Char -> [String]
split [] delim = [""]
split (x:xs) delim
	| x == delim = "" : rest
	| otherwise = (x : head rest) : tail rest
	where
		rest = split xs delim
 
decode :: [Char] -> [Int] -> [Char]
decode _ [] = []
decode cs (d:ds) = (cs !! (d - 1)) : decode cs ds

main  =
        getContents >>= \content ->
		let linesofFile = lines content
		in let splits1 = [isHappy (read lineOfFile::Int) | lineOfFile <- linesofFile]
		in let outputLines = [putStr (if y then "1" else "0") | y <- splits1]
		in seqn outputLines

seqn :: [IO a] -> IO ()
seqn [] = return ()
seqn (x : xs) = do
					x; putChar '\n'
					seqn xs

isHappy n = (last (take 100 (sumSumDigs n))) == 1

sumSumDigs n = n : sumSumDigs (sum [x^2|x<-(extractDigitsRev n)])

extractDigitsRev :: Int -> [Int]
extractDigitsRev 0 = [0]
extractDigitsRev n = extractDigitsRevRecur n

extractDigitsRevRecur :: Int -> [Int]
extractDigitsRevRecur 0 = []
extractDigitsRevRecur n = (n `mod` 10) : (extractDigitsRevRecur (n `div` 10))

{-
memoized_minSum :: Int -> Int -> Int -> Int
memoized_minSum n i j = (map minSum [0 ..] !!)
   where minSum  n i j 	| i == n - 1 && j == n - 1	= testData ! (i,j)
						| i == n - 1				= testData ! (i,j) + memoized_minSum 
				|               j == n - 1	= do
												mem <- minSum arr n (i + 1) j
												return (arr ! (i,j) + mem)
				| otherwise					= do
												mem1 <- minSum arr n i (j + 1)
												mem2 <- minSum arr n (i + 1) j
												return (arr ! (i,j) + min mem1 mem2)
-}

--calcAll :: Int -> [Int] -> Int
--calcAll n ds = min (calcMin) ()head [calcMin n arr mp (fst coord) (snd coord) | coord <- reverse $ range ((0,0),(n-1,n-1))]
--				where
--					arr = matrix n ds

--calcMin :: Int -> Array (Int, Int) Int -> Map.Map (Int,Int) Int -> Int -> Int -> Map.Map (Int,Int) Int
--calcMin n arr mp i j | i == n - 1 && j == n -1 = Map.insert (i,j) (arr ! (i,j)) mp
--					 | i == n - 1              = Map.insert (i,j) (arr ! (i,j) + mp Map.! (i,j+1)) mp
--					 |              j == n - 1 = Map.insert (i,j) (arr ! (i,j) + mp Map.! (i+1,j)) mp
--					 | otherwise			   = Map.insert (i,j) (arr ! (i,j) + min (mp Map.! (i,j+1)) (mp Map.! (i+1,j))) mp
-}
{-
import Data.Array

val :: Array (Int,Int) Char
val = listArray ((0,0),(2,4)) ['a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l', 'm', 'n', 'o']
main  =
        getContents >>= \content ->
		let linesofFile = lines content
		in let splits1 = [split lineOfFile ';' | lineOfFile <- linesofFile]
		in let splits2 = [solve (read (head s1)::Int) (read (head (tail s1))::Int) (split (head (tail (tail s1))) ' ') | s1 <- splits1]
		in let outputLines = [putStr (foldResult y) | y <- splits2]
		in seqn outputLines

foldResult :: [[Char]] -> [Char]
foldResult [] = ""
foldResult [x] = x
foldResult (x:xs) = x ++ " " ++ foldResult xs

seqn :: [IO a] -> IO ()
seqn [] = return ()
seqn (x : xs) = do
					x; putChar '\n'
					seqn xs

split :: String -> Char -> [String]
split [] delim = [""]
split (x:xs) delim
	| x == delim = "" : rest
	| otherwise = (x : head rest) : tail rest
	where
		rest = split xs delim
 
solve :: Int -> Int -> [a] -> [a]
solve n m ds = sideWalks arr n m 0 0
				where
					arr = listArray ((0,0),(n - 1, m - 1)) ds

sideWalks :: Array (Int, Int) a -> Int -> Int -> Int -> Int -> [a]
sideWalks _   0 _ _ _ = []
sideWalks _   _ 0 _ _ = []
sideWalks arr 1 1 x y = [arr ! (x,y)]
sideWalks arr n 1 x y = [arr ! (x + i,y) | i <- [0..n - 1]]
sideWalks arr 1 m x y = [arr ! (x,y + i) | i <- [0..m - 1]]
sideWalks arr n m x y = sideWalk arr n m x y ++ sideWalks arr (n - 2) (m - 2) (x + 1) (y + 1)

sideWalk :: Array (Int, Int) a -> Int -> Int -> Int -> Int -> [a]
sideWalk arr n m x y = [arr ! (x,y + i) | i <- [0..m - 1]] ++
					   [arr ! (x + i,y + m - 1) | i <- [1..n - 1]] ++
					   [arr ! (x + n - 1,y + i) | i <- reverse [0..m - 2]] ++
					   [arr ! (x + i,y) | i <- reverse [1..n - 2]]

import Data.Array

main  =
        getContents >>= \content ->
		let linesofFile = lines content
		in let splits1 = [split lineOfFile ';' | lineOfFile <- linesofFile]
		in let splits2 = [validate (read (head s1)::Int) (map (\x -> read x::Int) (split (head (tail s1)) ',')) | s1 <- splits1]
		in let outputLines = [putStr (show y) | y <- splits2]
		in seqn outputLines

seqn :: [IO a] -> IO ()
seqn [] = return ()
seqn (x : xs) = do
					x; putChar '\n'
					seqn xs


split :: String -> Char -> [String]
split [] delim = [""]
split (x:xs) delim
	| x == delim = "" : rest
	| otherwise = (x : head rest) : tail rest
	where
		rest = split xs delim
 

validate :: Int -> [Int] -> Bool
validate n items = (areEqual cs) && (areEqual rs) && (areEqual bs)
					where
						val = sudoku n items
						cs = itemSums n val colSum
						rs = itemSums n val rowSum
						bs = blockSums n val
					
sudoku :: Int -> [Int] -> Array (Int,Int) Int
sudoku n ds = listArray ((0,0),(n - 1, n - 1)) ds

areEqual :: [Int] -> Bool
areEqual [] = True
areEqual [a] = True
areEqual (x1:x2:xs) = if x1 /= x2 then False else areEqual (x2:xs)

itemSums :: Int -> Array (Int, Int) Int -> (Int -> Int -> Array (Int,Int) Int -> Int) -> [Int]
itemSums n arr itemSum = [itemSum n i arr | i <- [0.. n - 1]]

colSum :: Int -> Int -> Array (Int,Int) Int -> Int
colSum n col arr = sum [arr ! (i,col) | i <- [0..n-1]]

rowSum :: Int -> Int -> Array (Int,Int) Int -> Int
rowSum n row arr = sum [arr ! (row,i) | i <- [0..n-1]]

blockSums :: Int -> Array (Int,Int) Int -> [Int]
blockSums n arr = [blockSum block x y arr | x <- bs, y <- bs]
					where
						block = truncate $ sqrt $ fromIntegral n
						bs = [0, block..(n - block)]
					
blockSum :: Int -> Int -> Int -> Array (Int,Int) Int -> Int
blockSum b x y arr = sum [arr ! (j,i) | i <- [y..y + b - 1], j <- [x..x + b -1]]

main  =
        getContents >>= \content ->
		let linesofFile = lines content
		in let splits1 = [poppop (foldl push [] (map (\x -> read x::Int) (words lineOfFile))) | lineOfFile <- linesofFile]
		in let outputLines = [putStr (foldResult (map (\x -> show x) y)) | y <- splits1]
		in seqn outputLines

seqn :: [IO a] -> IO ()
seqn [] = return ()
seqn (x : xs) = do
					x; putChar '\n'
					seqn xs

foldResult :: [[Char]] -> [Char]
foldResult [] = ""
foldResult [x] = x
foldResult (x:xs) = x ++ " " ++ foldResult xs

poppop :: [Int] -> [Int]
poppop [] = []
poppop [n] = [n]
poppop (n1:n2:ns) = n1 : poppop ns

push :: [Int] -> Int -> [Int]
push ns n = n : ns

import Control.Monad.Fix

main  =
        getContents >>= \content ->
		let linesofFile = lines content
		in let splits1 = [read lineOfFile::Int | lineOfFile <- linesofFile]
		in let outputLines = [putStr (show (fibMemo y)) | y <- splits1]
		in seqn outputLines

seqn :: [IO a] -> IO ()
seqn [] = return ()
seqn (x : xs) = do
					x; putChar '\n'
					seqn xs

fib :: (Int -> Integer) -> Int -> Integer
fib f 0 = 1
fib f 1 = 1
fib f n = f (n - 1) + f (n - 2)

memoize :: (Int -> a) -> (Int -> a)
memoize f = (map f [0 ..] !!)

fibMemo :: Int -> Integer
fibMemo = fix (memoize . fib)

import Data.Array

main  =
        getContents >>= \content ->
		let linesofFile = lines content
		in let splits1 = [split x ';' | x <- linesofFile]
		in let splits2 = [ sumAllMines (read (head (split (head split1) ','))::Int) (read (head (tail (split (head split1) ',')))::Int) (head (tail split1)) | split1 <- splits1]
		in let outputLines = [putStr (foldResult y) | y <- splits2]
		in seqn outputLines

foldResult :: [[Char]] -> [Char]
foldResult [] = ""
foldResult [x] = x
foldResult (x:xs) = x ++ foldResult xs

seqn :: [IO a] -> IO ()
seqn [] = return ()
seqn (x : xs) = do
					x; putChar '\n'
					seqn xs

split :: String -> Char -> [String]
split [] delim = [""]
split (x:xs) delim
	| x == delim = "" : rest
	| otherwise = (x : head rest) : tail rest
	where
		rest = split xs delim
 
sumAllMines :: Int -> Int -> [Char] -> [[Char]]
sumAllMines m n ds = map (sumMines m n (mineField m n ds)) [0..m*n - 1] 

mineField :: Int -> Int -> [Char] -> Array Int Char
mineField m n ds = listArray (0,m*n - 1) ds

isMine :: Int -> Int -> Int -> Int -> Array Int Char -> Int
isMine m n i j fld	| i < 0 || i >= m = 0
					| j < 0 || j >= n = 0
					| otherwise = if fld ! (i * n + j) == '*' then 1 else 0

sumMines :: Int -> Int -> Array Int Char -> Int -> [Char]
sumMines m n fld i =if fld!i == '*' then "*" else show ((isMine m n (row-1) (col-1) fld)
												+ (isMine m n (row-1) (col-0) fld)
												+ (isMine m n (row-1) (col+1) fld)
												+ (isMine m n (row-0) (col-1) fld)
												+ (isMine m n (row-0) (col+1) fld)
												+ (isMine m n (row+1) (col-1) fld)
												+ (isMine m n (row+1) (col-0) fld)
												+ (isMine m n (row+1) (col+1) fld))
					where
						row = i `div` n
						col = i `mod` n
						
import Data.Array

main  =
        getContents >>= \content ->
		let linesofFile = lines content
		in let splits1 = [split x ';' | x <- linesofFile]
		in let splits2 = [ longestCommon (head split1) (head (tail split1)) | split1 <- splits1]
		in let outputLines = [putStr y | y <- splits2]
		in seqn outputLines

seqn :: [IO a] -> IO ()
seqn [] = return ()
seqn (x : xs) = do
					x; putChar '\n'
					seqn xs

split :: String -> Char -> [String]
split [] delim = [""]
split (x:xs) delim
	| x == delim = "" : rest
	| otherwise = (x : head rest) : tail rest
	where
		rest = split xs delim
 
longestCommon xs ys = a!(0,0)
	where
		n = length xs
		m = length ys
		a = array ((0,0),(n,m)) $ l1 ++ l2 ++ l3
		l1 = [((i,m),[]) | i <- [0..n]]
		l2 = [((n,j),[]) | j <- [0..m - 1]]
		l3 = [((i,j), f x y i j) | (x,i) <- zip xs [0..], (y,j) <- zip ys [0..]]
		f x y i j 
			| x == y    = x : a!(i+1,j+1)
			| otherwise = maxl (a!(i,j+1)) (a!(i+1,j))

maxl str1 str2 = if (length str1) > (length str2) then str1 else str2

--memo :: ([Char] -> [Char] -> a) -> [[a]]
--memo f = map (\x -> map (f x) [0..]) [0..]

gw :: [Char] -> [Char] -> [Char] -> [Char] -> Int
gw _ _ [] lst = 0
gw _ _ lst [] = 0
gw str1 str2 (x:xs) (y:ys) = if x == y then 1 + (fastgw str1 str2 xs ys) else max (fastgw str1 str2 (x:xs) ys) (fastgw str1 str2 xs (y:ys))

test1 str1 = (\x -> map (gw str1 str1 (drop x str1)) [0..])

gwstore :: [Char] -> [Char] -> [[Int]]
gwstore str1 str2 = [[1]]
-- \y -> map ( (drop y str2)) [0..])
-- \y -> map  [0..]

fastgw :: [Char] -> [Char] -> [Char] -> [Char] -> Int
fastgw str1 str2 x y = (gwstore str1 str2) !! (length x) !! (length y)
-}
{-
import Control.Monad.Fix

longestCommon :: ([Char] -> [Char] -> Int) -> [Char] -> [Char] -> Int
longestCommon f [] lst = 0
longestCommon f lst [] = 0
longestCommon f (c:cs) (d:ds) = if c == d then (1 + f cs ds) else max (f cs (d:ds)) (f (c:cs) ds)

--longestCommonMemo :: [Char] -> [Char] -> Int
--longestCommonMemo = fix (memoize . longestCommon)

memo :: (Num a, Enum a) => (a -> b) -> [b]
memo f = map f (enumFrom 0)

--memo2 :: (Num a, Enum a) => (a -> b) -> [b]
memo2 f = map memo (memo f)

--memoize :: ([Char] -> [Char] -> a) -> ([Char] -> [Char] -> a)
--memoize f = memoAtom (memoAtom f)


memoize :: (Int -> Int -> a) -> [[a]]

lst :: [Char] -> [Char] -> Int
lst [] s = 0
lst s [] = 0
lst (c:cs) (d:ds) = if c == d then 1 + (fastlst cs ds) else max (fastlst (c:cs) ds) (fastlst cs (d:ds))

lststore :: [[Int]]
lststore = memoize lst

fastlst :: [Char] -> [Char] -> Int
fastlst x y = lststore !! x !! y
-}
{-
longestCommonMemo :: Int -> Integer
longestCommonMemo = fix (memoize . longestCommon)

longestCommon :: ([Char] -> [Char] -> Int) -> [Char] -> [Char] -> Int
longestCommon f [] lst = 0
longestCommon f lst [] = 0
--longestCommon f (c:cs) (d:ds) = if c == d then (1 + longestCommon cs ds) else (max (longestCommon cs (d:ds)) (longestCommon (c:cs) ds))

main  =
        getContents >>= \content ->
		let linesofFile = lines content
		in let splits1 = [split x ';' | x <- linesofFile]
		in let splits2 = [ longestCommon (head split1) (head (tail split1)) | split1 <- splits1]
		in let outputLines = [putStr y | y <- splits2]
		in seqn outputLines

seqn :: [IO a] -> IO ()
seqn [] = return ()
seqn (x : xs) = do
					x; putChar '\n'
					seqn xs

split :: String -> Char -> [String]
split [] delim = [""]
split (x:xs) delim
	| x == delim = "" : rest
	| otherwise = (x : head rest) : tail rest
	where
		rest = split xs delim

longestCommon :: [Char] -> [Char] -> [Char]
longestCommon [] lst = []
longestCommon lst [] = []
longestCommon (c:cs) (d:ds) = if c == d then (c : longestCommon cs ds) else (maxSize (longestCommon cs (d:ds)) (longestCommon (c:cs) ds))

maxSize :: [Char] -> [Char] -> [Char]
maxSize str1 str2 = if (length str1) > (length str2) then str1 else str2

import Control.Monad.Fix

--(.) :: (b -> c) -> (a -> b) -> a -> c
--(.) f g = \ x -> f (g x)
 
--fix :: (a -> a) -> a
--fix f = let x = f x in x

fib :: (Int -> Integer) -> (Int -> Integer)
fib f 0 = 1
fib f 1 = 1
fib f n = f (n - 1) + f (n - 2)

memoize :: (Int -> a) -> (Int -> a)
memoize f = (map f [0 ..] !!)

fibMemo :: Int -> Integer
fibMemo = fix (memoize . fib)

findGreater :: Int -> Int -> Int
findGreater m n = if n >= m then n else findGreater m (n * 2)

main  =
        getContents >>= \content ->
		let outputLines = [putStr (show (length content))]
		in seqn outputLines

seqn :: [IO a] -> IO ()
seqn [] = return ()
seqn (x : xs) = do
					x; putChar '\n'
					seqn xs

import Data.Map hiding (map)

main  =
        getContents >>= \content ->
		let linesofFile = lines content
		in let splits1 = [map (\item -> words item) (split1 x) | x <- linesofFile]
		in let splits2 = [ translateWords s | s <- splits1]
		in let outputLines = [putStr (foldResult y) | y <- splits2]
		in seqn outputLines

foldResult :: [[Char]] -> [Char]
foldResult [] = ""
foldResult [x] = x
foldResult (x:xs) = x ++ " " ++ foldResult xs

seqn :: [IO a] -> IO ()
seqn [] = return ()
seqn (x : xs) = do
					x; putChar '\n'
					seqn xs

split1 :: String -> [String]
split1 [] = [""]
split1 [x] = [[x]]
split1 (x:xs)
	| x == ' ' && (head xs) == ' ' = "" : rest
	| otherwise = (x : head rest) : tail rest
	where
		rest = split1 xs

translateWords :: [[[Char]]] -> [[Char]]
translateWords [] = []
translateWords (ws:wss) = translateWord ws : translateWords wss

translateWord :: [[Char]] -> [Char]
translateWord [] = []
translateWord (w:ws) = symbols ! w : translateWord ws

symbols :: Map [Char] Char
symbols = fromList [(".-",'A'), ("-...", 'B'), ("-.-.", 'C'), ("-..", 'D'), (".", 'E'), ("..-.", 'F'), ("--.", 'G'), ("....", 'H'), ("..", 'I'), (".---", 'J'), ("-.-", 'K'), (".-..", 'L'), ("--", 'M'), ("-.", 'N'), ("---", 'O'), (".--.", 'P'), ("--.-", 'Q'), (".-.", 'R'), ("...", 'S'), ("-", 'T'), ("..-", 'U'), ("...-", 'V'), (".--", 'W'), ("-..-", 'X'), ("-.--", 'Y'), ("--..", 'Z'), (".----", '1'), ("..---", '2'), ("...--", '3'), ("....-", '4'), (".....", '5'), ("-....", '6'), ("--...", '7'), ("---..", '8'), ("----.", '9'), ("-----", '0')]

main  =
        getContents >>= \content ->
		let linesofFile = lines content
		in let splits1 = [words lineOfFile | lineOfFile <- linesofFile]
		in let splits2 = [ longestRecur split1 "" | split1 <- splits1]
		in let outputLines = [putStr y | y <- splits2]
		in seqn outputLines

seqn :: [IO a] -> IO ()
seqn [] = return ()
seqn (x : xs) = do
					x; putChar '\n'
					seqn xs

longestRecur :: [[Char]] -> [Char] -> [Char]
longestRecur [] max = max
longestRecur (item:items) max = if (length item) > (length max) then longestRecur items item else longestRecur items max

main  =
        getContents >>= \content ->
		let linesofFile = lines content
		in let splits1 = [map (\item -> split item ',') (split x ';') | x <- linesofFile]
		in let splits2 = [ foldl (\z x -> if x == [""] then z else (read (head(tail x))::Int) : z) [] split1 | split1 <- splits1]
		in let result = [ sortDiff split2 | split2 <- splits2]
		in let outputLines = [if y == [] then putStr "" else (putStr (foldResult y)) | y <- result]
		in seqn outputLines

foldResult :: [Int] -> [Char]
foldResult [] = ""
foldResult [x] = (show x)
foldResult (x:xs) = (show x) ++ "," ++ foldResult xs

seqn :: [IO a] -> IO ()
seqn [] = return ()
seqn (x : xs) = do
					x; putChar '\n'
					seqn xs

split :: String -> Char -> [String]
split [] delim = [""]
split (x:xs) delim
	| x == delim = "" : rest
	| otherwise = (x : head rest) : tail rest
	where
		rest = split xs delim

sortDiff :: [Int] -> [Int]
sortDiff [] = []
sortDiff [x] = [x]
sortDiff lst = head slist : (diff slist)
				where 
					slist = qsort lst

diff :: [Int] -> [Int]
diff [] = []
diff [x,y] = [y-x]
diff (x:xs) = (head xs - x) : diff xs

qsort [] = []
qsort (c:cs) = qsort smaller ++ [c] ++ qsort larger
				where
					smaller = [x | x <- cs, x <= c]
					larger = [x | x <- cs, x > c]

main  =
        getContents >>= \content ->
		let linesofFile = lines content
		in let numLinePairs = [(split x ';') | x <- linesofFile]
		in let seqCount = [ interset (map (\s -> read s::Int) (split (head numLinePair) ',')) (map (\s -> read s::Int) (split (head (tail numLinePair)) ',')) | numLinePair <- numLinePairs]
		in let outputLines = [if y == [] then putStr "" else (putStr (foldResult y)) | y <- seqCount]
		in seqn outputLines

foldResult :: [Int] -> [Char]
foldResult [] = ""
foldResult [x] = (show x)
foldResult (x:xs) = (show x) ++ "," ++ foldResult xs

seqn :: [IO a] -> IO ()
seqn [] = return ()
seqn (x : xs) = do
					x; putChar '\n'
					seqn xs

split :: String -> Char -> [String]
split [] delim = [""]
split (x:xs) delim
	| x == delim = "" : rest
	| otherwise = (x : head rest) : tail rest
	where
		rest = split xs delim

interset :: [Int] -> [Int] -> [Int]
interset [] _ = []
interset _ [] = []
interset (x:xs) (y:ys) = case compare x y of
							EQ -> x : interset xs ys
							LT -> interset xs (y:ys)
							GT -> interset (x:xs) ys

import Data.Array

getDataArrayItems :: Int -> Int -> Array Int Char -> [Char]
getDataArrayItems row col items = if inRange (bounds items) (row * col) then 

dataArray :: Int -> Int -> [Char] -> Array Int Char
dataArray rows cols entries = listArray (0, rows * cols - 1) entries

--getCoords :: Int -> Int -> [(Int, Int)]
--getCoords row col | row == 0 && col == 0 = [(

main  =
        getContents >>= \content ->
		let linesofFile = lines content
		in let numLinePairs = [(split x '|') | x <- linesofFile]
		in let seqCount = [zipWith (*) (map (\s -> read s::Int) (words (head numLinePair))) (map (\s -> read s::Int) (words (head (tail numLinePair))))| numLinePair <- numLinePairs]
		in let outputLines = [putStr (foldResult y) | y <- seqCount]
		in seqn outputLines

foldResult :: [Int] -> [Char]
foldResult [n] = show n
foldResult (n:ns) = (show n) ++ " " ++ foldResult ns

seqn :: [IO a] -> IO ()
seqn [] = return ()
seqn (x : xs) = do
					x; putChar '\n'
					seqn xs

split :: String -> Char -> [String]
split [] delim = [""]
split (x:xs) delim
	| x == delim = "" : rest
	| otherwise = (x : head rest) : tail rest
	where
		rest = split xs delim

import Data.Array

main  =
        getContents >>= \content ->
		let linesofFile = lines content
		in let numLinePairs = [(split x ';') | x <- linesofFile]
		in let seqCount = [ searchSums (map (\s -> read s::Int) (split (head numLinePair) ',')) (read (head (tail numLinePair))::Int) | numLinePair <- numLinePairs]
		in let outputLines = [if y == [] then putStr "NULL" else (putStr (foldResult y)) | y <- seqCount]
		in seqn outputLines

foldResult :: [(Int,Int)] -> [Char]
foldResult [] = ""
foldResult [(x,y)] = (show x) ++ "," ++ (show y)
foldResult ((x,y):xs) = (show x) ++ "," ++ (show y) ++ ";" ++ foldResult xs

seqn :: [IO a] -> IO ()
seqn [] = return ()
seqn (x : xs) = do
					x; putChar '\n'
					seqn xs

split :: String -> Char -> [String]
split [] delim = [""]
split (x:xs) delim
	| x == delim = "" : rest
	| otherwise = (x : head rest) : tail rest
	where
		rest = split xs delim

searchSums :: [Int] -> Int -> [(Int,Int)]
searchSums [] _ = []
searchSums (n:ns) s = case searchItem items (s - n) of
						Nothing -> searchSums ns s
						Just x -> (n, items ! x) : searchSums ns s
						where
							items = (searchDomain ((length ns) - 1) ns)

searchItem :: Array Int Int -> Int -> Maybe Int
searchItem items item = binarySearchArray items item

searchDomain count items = listArray (0,count) items
 
binarySearchArray :: (Ix i, Integral i, Ord e) => Array i e -> e -> Maybe i
binarySearchArray a x = binarySearch p (bounds a)
							where
								p m = x `compare` (a ! m)
  
binarySearch :: Integral a => (a -> Ordering) -> (a, a) -> Maybe a
binarySearch p (low,high) 
  | high < low = Nothing
  | otherwise = 
      let mid = (low + high) `div` 2 in 
      case p mid of
        LT -> binarySearch p (low, mid-1)
        GT -> binarySearch p (mid+1, high)
        EQ -> Just mid

import Data.Array

rowCount :: Int -> Array Int Int -> [Int]

board :: (Ix a, Int a) => a -> [Int] -> Array a Int
board n nums = listArray (0,n*n - 1) nums

main  =
        getContents >>= \content ->
		let linesofFile = lines content
		in let nums = [(split x ',') | x <- linesofFile]
		in let seqCount = [trimCharsRecur (head cs) (remChars (trim (head (tail cs)))) | cs <- nums]
		in let outputLines = [putStr y | y <- seqCount]
		in seqn outputLines

seqn :: [IO a] -> IO ()
seqn [] = return ()
seqn (x : xs) = do
					x; putChar '\n'
					seqn xs

split :: String -> Char -> [String]
split [] delim = [""]
split (x:xs) delim
	| x == delim = "" : rest
	| otherwise = (x : head rest) : tail rest
	where
		rest = split xs delim

trimCharsRecur :: [Char] -> Array Int Bool -> [Char]
trimCharsRecur [] _ = []
trimCharsRecur (c:cs) ds = if (ds ! (fromEnum c)) then trimCharsRecur cs ds else (c : trimCharsRecur cs ds)

genCharSeq :: [Char] -> [Bool]
genCharSeq [] = [ False | _ <- [0..255]]
genCharSeq lst = genSeq (map (\c -> fromEnum c) lst)

genSeq :: [Int] -> [Bool]
genSeq lst = [False | _ <- [0..(head lst) - 1]] ++ genSeqReq lst

genSeqReq :: [Int] -> [Bool]
genSeqReq [x] = genRange x 255
genSeqReq (x:xs) = (genRange x ((head xs) - 1)) ++ genSeqReq xs

genRange :: Int -> Int -> [Bool]
genRange n m = True : [False | x <- [n + 1..m]]

qsort [] = []
qsort (c:cs) = qsort smaller ++ [c] ++ qsort larger
				where
					smaller = [x | x <- cs, x <= c]
					larger = [x | x <- cs, x > c]

remChars chars = listArray (0,255) (genCharSeq chars)

trim :: [Char] -> [Char]
trim [] = []
trim (c:cs) = if c == ' ' then trim cs else c : trim cs

main  =
        getContents >>= \content ->
		let linesofFile = lines content
		in let nums = [read lineOfFile::Int | lineOfFile <- linesofFile]
		in let outputLines = [putStr (show (sum nums))]
		in seqn outputLines

seqn :: [IO a] -> IO ()
seqn [] = return ()
seqn (x : xs) = do
					x; putChar '\n'
					seqn xs

nextToLast :: [[Char]] -> [Char]
nextToLast [] = []
nextToLast [_] = []
nextToLast lst = lst !! ((length lst) - 2)

main  =
        getContents >>= \content ->
		let linesOfFile = lines content
		in let swappedCaseLines = [swapCase lineOfFile | lineOfFile <- linesOfFile]
		in let outputLines = [putStr y | y <- swappedCaseLines]
		in seqn outputLines

seqn :: [IO a] -> IO ()
seqn [] = return ()
seqn (x : xs) = do
					x; putChar '\n'
					seqn xs

swapCase :: [Char] -> [Char]
swapCase [] = []
swapCase (c:cs) = swapCharCase c : swapCase cs

swapCharCase :: Char -> Char
swapCharCase c 	| ((fromEnum 'a') <= (fromEnum c)) && ((fromEnum c) <= (fromEnum 'z')) = (toEnum ((fromEnum c) - (fromEnum 'a') + (fromEnum 'A')))
				| ((fromEnum 'A') <= (fromEnum c)) && ((fromEnum c) <= (fromEnum 'Z')) = (toEnum ((fromEnum c) - (fromEnum 'A') + (fromEnum 'a')))
				| otherwise = c
				
import Data.List (insert)
import Text.Regex.Posix

pat = "(foo[a-z]*bar|quux)"

res = "i foobarbar a quux" =~ pat :: ([] (Int,Int))

isSquare :: Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Bool
isSquare x1 y1 x2 y2 x3 y3 x4 y4 = (a1 == a2)
								&& (a3 == a4)
								&& (b1 == b3)
								&& (b2 == b4)
								&& ((a3 - a1) == (b4 - b3))
									where
										lst = insertionSort[(x1,y1),(x2,y2),(x3,y3),(x4,y4)]
										(a1,b1) = head lst
										(a2,b2) = head (tail lst)
										(a3,b3) = head (tail (tail lst))
										(a4,b4) = head (tail (tail (tail lst)))

insertionSort :: Ord a => [a] -> [a]
insertionSort = foldr insert []

main = putStr (show primesSum)

primesSum :: Int
primesSum = sum (take 1000 primesRecur)

primesRecur :: [Int]
primesRecur = sieve [2..]
				where sieve (p:xs) = p : sieve [x|x <- xs, x `mod` p > 0]

main  =
        getContents >>= \content ->
		let linesofFile = lines content
		in let nums = [read lineOfFile::Int | lineOfFile <- linesofFile]
		in let seqCount = [map (\n -> show n) (primes num) | num <- nums]
		in let outputLines = [putStr (foldStrings y) | y <- seqCount]
		in seqn outputLines

foldStrings :: [[Char]] -> [Char]
foldStrings [] = ""
foldStrings (string:[]) = string
foldStrings (string:strings) = string ++ "," ++ (foldStrings strings)
		
seqn :: [IO a] -> IO ()
seqn [] = return ()
seqn (x : xs) = do
					x; putChar '\n'
					seqn xs

split :: String -> Char -> [String]
split [] delim = [""]
split (x:xs) delim
	| x == delim = "" : rest
	| otherwise = (x : head rest) : tail rest
	where
		rest = split xs delim

primes :: Int -> [Int]
primes n| n < 3 = []
		| otherwise = sieve [2..(n-1)]
						where
							sieve (p:[]) = [p]
							sieve (p:xs) = p : sieve [x|x <- xs, x `mod` p > 0]

primesBetween :: Int -> Int -> Int
primesBetween n m 	|(last nPrimes) == n = (length mPrimes) - (length nPrimes) + 1
					|otherwise = (length mPrimes) - (length nPrimes)
					where
						mPrimes = primes m
						nPrimes = primes n

main  =
        getContents >>= \content ->
		let linesofFile = lines content
		in let nums = [map (\str -> read str::Int) (split x ',') | x <- linesofFile]
		in let seqCount = [overlap (coords !! 0) (coords !! 1) (coords !! 2) (coords !! 3) (coords !! 4) (coords !! 5) (coords !! 6) (coords !! 7) | coords <- nums]
		in let outputLines = [putStr (show y) | y <- seqCount]
		in seqn outputLines

seqn :: [IO a] -> IO ()
seqn [] = return ()
seqn (x : xs) = do
					x; putChar '\n'
					seqn xs

split :: String -> Char -> [String]
split [] delim = [""]
split (x:xs) delim
	| x == delim = "" : rest
	| otherwise = (x : head rest) : tail rest
	where
		rest = split xs delim
	
overlap :: Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Bool
overlap x1 y1 x2 y2 a1 b1 a2 b2 = (pointInRectangle x1 y1 x2 y2 a1 b1)
								||(pointInRectangle x1 y1 x2 y2 a2 b2)
								||(pointInRectangle x1 y1 x2 y2 a1 b2)
								||(pointInRectangle x1 y1 x2 y2 a2 b1)
								||(pointInRectangle a1 b1 a2 b2 x1 y1)
								||(pointInRectangle a1 b1 a2 b2 x2 y2)
								||(pointInRectangle a1 b1 a2 b2 x1 y2)
								||(pointInRectangle a1 b1 a2 b2 x2 y1)

pointInRectangle :: Int -> Int -> Int -> Int -> Int -> Int -> Bool
pointInRectangle xl yl xr yr a b = (a >= xl) && (a <= xr) && (b <= yl) && (b >= yr)

main  =
        getContents >>= \content ->
		let linesofFile = lines content
		in let stringSubstringLines = [splitLine (split x ',') | x <- linesofFile]
		in let seqCount = [distinctSequenceCount (head stringPair) (head (tail stringPair)) | stringPair <- stringSubstringLines]
		in let outputLines = [putStr (show y) | y <- seqCount]
		in seqn outputLines

seqn :: [IO a] -> IO ()
seqn [] = return ()
seqn (x : xs) = do
					x; putChar '\n'
					seqn xs

splitLine :: [String] -> [String]
splitLine inputStrings = [x | x <- inputStrings]

split :: String -> Char -> [String]
split [] delim = [""]
split (x:xs) delim
	| x == delim = "" : rest
	| otherwise = (x : head rest) : tail rest
	where
		rest = split xs delim
					
distinctSequenceCount :: [Char] -> [Char] -> Int
distinctSequenceCount string [] = 0
distinctSequenceCount string seq = length (extractWholeString [string] seq)

extractWholeString :: [[Char]] -> [Char] -> [[Char]]
extractWholeString [[]] chars = []
extractWholeString [] chars = []
extractWholeString strings [] = strings
extractWholeString strings (c:cs) = extractWholeString (extractMultipleSubstrings strings c) cs

extractMultipleSubstrings :: [[Char]] -> Char -> [[Char]]
extractMultipleSubstrings [] char = []
extractMultipleSubstrings [[]] char = []
extractMultipleSubstrings (cs:css) char = (extractSubstrings cs char) ++ (extractMultipleSubstrings css char)

extractSubstrings :: [Char] -> Char -> [[Char]]
extractSubstrings [] char = []
extractSubstrings (c:cs) char	| c == char = cs : (extractSubstrings cs char)
								| True = (extractSubstrings cs char)

main  =
        getContents >>= \content ->
		let linesofFile = lines content
		in let nums = [process (words lineOfFile) | lineOfFile <- linesofFile]
		in let outputLines = [putStr (show num) | num <- nums ]
		in seqn outputLines

seqn :: [IO a] -> IO ()
seqn [] = return ()
seqn (x : xs) = do
					x; putChar '\n'
					seqn xs

items :: [([Char], Integer)]
items = [("zero",0), ("one",1), ("two",2), ("three",3), ("four",4), ("five",5), ("six",6), ("seven",7), ("eight",8), ("nine",9),
		 ("ten",10), ("eleven",11), ("twelve",12), ("thirteen",13), ("fourteen",14), ("fifteen",15), ("sixteen",16), ("seventeen",17), ("eighteen",18), ("nineteen",19),
		 ("twenty",20), ("thirty",30), ("forty",40), ("fifty",50), ("sixty",60), ("seventy",70), ("eighty",80), ("ninety",90)]
		 
hundred = "hundred"
thousand = "thousand"
million = "million"

process :: [[Char]] -> Integer
process [] = 0
process ("negative":xs) = -1 * (sum (processRecur xs []))
process lst = sum (processRecur lst [])

processRecur :: [[Char]] -> [Integer] -> [Integer]
processRecur [] temps = [sum temps]
processRecur (text:texts) temps | text == million = ((sum temps) * 1000000) : (processRecur texts [])
								| text == thousand = ((sum temps) * 1000) : (processRecur texts [])
								| text == hundred = processRecur texts [((sum temps) * 100)]
								| otherwise = processRecur texts ((getItemValue items text) : temps)

getItemValue :: [([Char], Integer)] -> [Char] -> Integer
getItemValue [] name = 0
getItemValue (r:rs) name = if (name == (fst r)) then (snd r) else (getItemValue rs name)

--Points in circle

isInside :: Double -> Double -> Double -> Double -> Double -> Bool
isInside cx cy r x y = (distance cx cy x y) < r

distance :: Double -> Double -> Double -> Double -> Double
distance x1 y1 x2 y2 = sqrt ((x1 - x2)^2 + (y1 - y2)^2)


main  =
        getContents >>= \content ->
		let linesofFile = lines content
		in let nums = [(read lineOfFile::Int) | lineOfFile <- linesofFile]
		in let outputLines = [putStr (numToText num) | num <- nums ]
		in seqn outputLines

seqn :: [IO a] -> IO ()
seqn [] = return ()
seqn (x : xs) = do
					x; putChar '\n'
					seqn xs

ones = ["",), ("One",), ("Two",), ("Three",), ("Four",), ("Five",), ("Six",), ("Seven",), ("Eight",), ("Nine"]
tens = ["Ten",), ("Eleven",), ("Twelve",), ("Thirteen",), ("Fourteen",), ("Fifteen",), ("Sixteen",), ("Seventeen",), ("Eighteen",), ("Nineteen",), ("Twenty",), ("Thirty",), ("Forty",), ("Fifty",), ("Sixty",), ("Seventy",), ("Eighty",), ("Ninety"]
hundred = "Hundred"
thousand = "Thousand"
million = "Million"

numToText :: Int -> [Char]
numToText 0 = "ZeroDollars"
numToText n = (numToTextRecur n) ++ "Dollars"

numToTextRecur :: Int -> [Char]
numToTextRecur n
	| n >= 1000000 = (numToTextRecur (n `div` 1000000)) ++ million ++ (numToTextRecur (n `mod` 1000000))
	| n >= 1000 = (numToTextRecur (n `div` 1000)) ++ thousand ++ (numToTextRecur (n `mod` 1000))
	| n >= 100 = (numToTextRecur (n `div` 100)) ++ hundred ++ (numToTextRecur (n `mod` 100))
	| n >= 10 = tensToText n
	| otherwise = ones !! n

tensToText :: Int -> [Char]
tensToText n| n >= 10 && n <= 20 = tens !! (n - 10)
			| otherwise = (tens !! ((n `div` 10) + 8)) ++ (ones !! (n `mod` 10))

import Data.Array

type Vertex = Char
type Table a = Array Vertex a
type Graph = Table [Vertex]

buildG :: Bounds -> [Edge] -> Graph
buildG bnds es = accumArray (flip (:)) [] bnds es

graph = buildG ('a'",'j')
			[('a'",'j')",('a'",'g')",('b'",'i')",
			('b'",'a')",('c'",'h')",('c'",'e')",
			('e'",'j')",('e'",'h')",('e'",'d')",
			('f'",'i')",('g'",'f')",('g'",'b')]

import Data.Bits

main  =
        getContents >>= \content ->
		let linesofFile = lines content
		in let characters = [firstNonRepeated lineOfFile | lineOfFile <- linesofFile]
		in let outputLines = [putStr [character] | character <- characters ]
		in seqn outputLines

seqn :: [IO a] -> IO ()
seqn [] = return ()
seqn (x : xs) = do
					x; putChar '\n'
					seqn xs

firstNonRepeated :: [Char] -> Char
firstNonRepeated lst = head [x | x <- lst",), ("not (isMarked m2 x) ]
						where (_",m2) = markAll 0 0 lst

markAll :: Integer -> Integer -> [Char] -> (Integer",), ("Integer)
markAll mark1 mark2 [] = (mark1",mark2)
markAll mark1 mark2 (c:cs) = markAll m1 m2 cs
								where (m1",m2) = mark mark1 mark2 c

mark :: Integer -> Integer -> Char -> (Integer",), ("Integer)
mark mark1 mark2 c = if (isMarked mark1 c) then (mark1",), ("(shiftNum mark2 c)) else ((shiftNum mark1 c)",), ("mark2)

shiftNum :: Integer -> Char -> Integer
shiftNum num c = num .|. (1 `shiftL` (fromEnum c))

isMarked :: Integer -> Char -> Bool
isMarked num c = (num .&. (1 `shiftL` (fromEnum c))) /= 0

main  =
        getContents >>= \content ->
		let linesofFile = lines content
		in let sentences = [words lineOfFile | lineOfFile <- linesofFile]
		in let outputLines = [putStr (foldStrings (capitalize sentence)) | sentence <- sentences ]
		in seqn outputLines

foldStrings :: [[Char]] -> [Char]
foldStrings [] = ""
foldStrings (string:[]) = string
foldStrings (string:strings) = string ++ " " ++ (foldStrings strings)

seqn :: [IO a] -> IO ()
seqn [] = return ()
seqn (x : xs) = do
					x; putChar '\n'
					seqn xs

capitalize :: [[Char]] -> [[Char]]
capitalize [] = []
capitalize (w:ws) = (toUpper w) : capitalize ws

toUpper :: [Char] -> [Char]
toUpper [] = []
toUpper (c:cs) = if (isLower c) then ((toEnum (codeA + code - codea)):cs) else (c:cs)
					where
						code = fromEnum c
						codeA = fromEnum 'A'
						codea = fromEnum 'a'
						codeZ = fromEnum 'Z'

isLower :: Char -> Bool
isLower c = (codea <= code) && (code <= codez)
			where
				code = fromEnum c
				codea = fromEnum 'a'
				codez = fromEnum 'z'	
				

binarySearch :: Integral a => (a -> Ordering) -> (a",), ("a) -> Maybe a
binarySearch p (low",high) 
  | high < low = Nothing
  | otherwise = 
      let mid = (low + high) `div` 2 in 
      case p mid of
        LT -> binarySearch p (low",), ("mid-1)
        GT -> binarySearch p (mid+1",), ("high)
        EQ -> Just mid

main  =
        getContents >>= \content ->
		let linesofFile = lines content
		in let numPairs = [words lineOfFile | lineOfFile <- linesofFile]
		in let uniqeList = [palindromeCount [(read (head numPair)::Int)..(read (head (tail numPair))::Int)] | numPair <- numPairs]
		in let outputLines = [putStr (show row) | row <- uniqeList ]
		in seqn outputLines

seqn :: [IO a] -> IO ()
seqn [] = return ()
seqn (x : xs) = do
					x; putChar '\n'
					seqn xs

palindromeCount :: [Int] -> Int
palindromeCount range = sum [sum z | z <- (multiplyIncrements (countPalindromeIntervals range))] + zeroPalindromeCount range

zeroPalindromeCount :: [Int] -> Int
zeroPalindromeCount [] = 0
zeroPalindromeCount lst = sum (map (\n -> sum [1..n]) (countPalindromeIntervals lst))

multiplyIncrements :: [Int] -> [[Int]]
multiplyIncrements nums = [(multiplyIncrementsRecur nums z) | z <- [2",4..(length nums)]]

multiplyIncrementsRecur :: [Int] -> Int -> [Int]
multiplyIncrementsRecur lst step = if (length lst) <= step then [] else ((head lst) + 1) * ((lst !! step) + 1) : multiplyIncrementsRecur (tail lst) step

countPalindromeIntervals :: [Int] -> [Int]
countPalindromeIntervals nums = countPalindromeIntervalsRecur nums 0

countPalindromeIntervalsRecur :: [Int] -> Int -> [Int]
countPalindromeIntervalsRecur [] n = [n]
countPalindromeIntervalsRecur (x:xs) n = if isPalindrome x then n : countPalindromeIntervalsRecur xs 0 else countPalindromeIntervalsRecur xs (n + 1)

isPalindrome :: Int -> Bool
isPalindrome n = lst == (reverse lst)
					where lst = extractDigitsRev n

extractDigitsRev :: Int -> [Int]
extractDigitsRev 0 = [0]
extractDigitsRev n = extractDigitsRevRecur n

extractDigitsRevRecur :: Int -> [Int]
extractDigitsRevRecur 0 = []
extractDigitsRevRecur n = (n `mod` 10) : (extractDigitsRevRecur (n `div` 10))

main  =
        getContents >>= \content ->
		let linesofFile = lines content
		in let nums = [read lineOfFile::Int | lineOfFile <- linesofFile]
		in let uniqeList = [getRows num | num <- nums]
		in let outputLines = [putStr (foldStrings row) | row <- uniqeList ]
		in seqn outputLines
		
foldStrings :: [Int] -> [Char]
foldStrings [] = ""
foldStrings (num:[]) = show num
foldStrings (num:nums) = (show num) ++ " " ++ (foldStrings nums)

seqn :: [IO a] -> IO ()
seqn [] = return ()
seqn (x : xs) = do
					x; putChar '\n'
					seqn xs

getRows :: Int -> [Int]
getRows n = [z | row <- getRowsRecur n [1]",), ("z <- row]

getRowsRecur :: Int -> [Int] -> [[Int]]
getRowsRecur 0 _ = []
getRowsRecur n start = start : getRowsRecur (n - 1) (getRow start)

getRow :: [Int] -> [Int]
getRow nums = 1 : getRowRecur nums

getRowRecur :: [Int] -> [Int]
getRowRecur [a] = [a]
getRowRecur [a ",), ("b] = [a + b ",), ("b]
getRowRecur (x1:x2:xs) = (x1 + x2) : getRowRecur(x2:xs)

main  =
        getContents >>= \content ->
		let linesOfFile = lines content
		in let outputCount = read (head linesOfFile)::Int
		in let outputLines = printTop outputCount (qsort (tail linesOfFile))
		in seqn outputLines

printTop :: Int -> [[Char]] -> [IO ()]
printTop 0 _ = []
printTop _ [] = []
printTop n (cs:css) = (putStr cs) : printTop (n - 1) css

qsort [] = []
qsort (str:strs) = case str of
					[] -> qsort strs
					nonEmpty -> qsort [x | x <- strs",), ("(length x) > (length nonEmpty)] ++ [nonEmpty] ++ qsort [x | x <- strs",), ("(length x) <= (length nonEmpty)]


isArmstrong :: [Char] -> Bool
isArmstrong lst = (read lst::Int) == cubeSumDigits lst

cubeSumDigits :: [Char] -> Int
cubeSumDigits lst = cubeSumDigitsRecur lst (length lst)

cubeSumDigitsRecur :: [Char] -> Int -> Int
cubeSumDigitsRecur [] _ = 0;
cubeSumDigitsRecur (d:ds) digCount = (read [d]::Int)^digCount + cubeSumDigitsRecur ds digCount


main  =
        getContents >>= \content ->
		let linesofFile = lines content
		in let numLines = [hexToDecimal numStr | numStr <- linesofFile]
		in let outputLines = [putStr (show y) | y <- numLines]
		in seqn outputLines

seqn :: [IO a] -> IO ()
seqn [] 		= return ()
seqn (x : xs) = do
					x; putChar '\n'
					seqn xs

hexToDecimal :: [Char] -> Int
hexToDecimal lst = hexToDecimalRecur (reverse lst)

hexToDecimalRecur :: [Char] -> Int
hexToDecimalRecur [] = 0
hexToDecimalRecur (d:ds) = (hexDigitToDecimal d) + 16 * (hexToDecimalRecur ds)

hexDigitToDecimal :: Char -> Int
hexDigitToDecimal c = case c of
						x | ('0' <= x) && (x <= '9') -> read [x]::Int
						x | ('a' <= x) && (x <= 'f') -> (fromEnum x) - (fromEnum 'a') + 10

main  =
        getContents >>= \content ->
		let linesofFile = lines content
		in let numLines = [split x '",' | x <- linesofFile]
		in let uniqeList = [unique numLine | numLine <- numLines]
		in let outputLines = [putStr (foldStrings y) | y <- uniqeList]
		in seqn outputLines

foldStrings :: [[Char]] -> [Char]
foldStrings [] = ""
foldStrings (string:[]) = string
foldStrings (string:strings) = string ++ ""," ++ (foldStrings strings)

split :: String -> Char -> [String]
split [] delim = [""]
split (x:xs) delim
	| x == delim = "" : rest
	| otherwise = (x : head rest) : tail rest
	where
		rest = split xs delim
				
seqn :: [IO a] -> IO ()
seqn [] 		= return ()
seqn (x : xs) = do
					x; putChar '\n'
					seqn xs

unique :: [[Char]] -> [[Char]]
unique [] = []
unique [a] = [a]
unique (a:b:cs) = if a == b then unique (b:cs) else a : unique(b:cs)

foldStrings :: [[Char]] -> [Char]
foldStrings [] = ""
foldStrings (string:[]) = string
foldStrings (string:strings) = string ++ ""," ++ (foldStrings strings)

numSet = ["0"","1"","abc"",), (""def"",), (""ghi"",), (""jkl"",), (""mno"",), (""pqrs"",), (""tuv"",), (""wxyz"]

buildPhoneSet :: [Char] -> [[Char]]
buildPhoneSet [] = []
buildPhoneSet (c:[]) = [[d] | d <- (numSet !! (read [c]::Int))]
buildPhoneSet (c:cs) = [z | x <- buildPhoneSet (c:[])",), ("z <- [x ++ set2 | set2 <- buildPhoneSet cs]]

board :: [[Char]]
board = [ "ABCE"",), (""SFCS"",), (""ADEE" ]

findCharRecur :: Char -> [Char] -> Int -> [Int]
findCharRecur _ [] _ = []
findCharRecur x (c:cs) tracker = if x == c then (tracker : findCharRecur x cs (tracker + 1)) else findCharRecur x cs (tracker + 1)

findRecur :: Char -> [[Char]] -> Int -> [(Int",Int)]
findRecur _ [] _ = []
findRecur c (row:rows) tracker = [(tracker",), ("i) | i <- (findCharRecur c row 0)] ++ findRecur c rows (tracker + 1)

getSibling :: (Int",Int) -> [[Char]] -> Int -> Int -> [(Int",), ("Int",), ("Char)]
getSibling _ [] _ _ = []
getSibling (i",j) b colCount rowCount| i < 0 || j < 0 = []
									| i >= rowCount || j >= colCount = []
									| 

--findNeighbor :: (Int",Int) -> Char -> [[Char]] -> [(Int",), ("Int)]
--findNeighbor _ _ [] = []
--findNeighbor (i",j) c (row:rows)

-}
