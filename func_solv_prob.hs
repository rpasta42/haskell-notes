import Data.List
import System.IO

--reverse polish notation

--10 4 3 + 2 * -
--1: 10 4 3 +
--2: 10 7
--3: 10 7 2 *
--4: 10 14
--5: 10 14 -
--6: -4

--my code

newStack = []

pushStack :: a -> [a] -> [a]
pushStack x xs = x:xs

popStack :: [a] -> (a, [a])
popStack (x:xs) = (x, xs)

getRpnList = words
isOp x = or $ map (==x) ["+", "-", "*"]

getOp "+" = (+)
getOp "-" = (-)
getOp "*" = (*)


solveRPN :: (Read a, Num a) => String -> a
solveRPN str = helper (getRpnList str) newStack

helper :: (Read a, Num a) => [String] -> [a] -> a
helper [] stack = fst . popStack $ stack
helper (x:xs) stack
   | isOp x =
      let (operand2, stack1) = popStack stack
          (operand1, stack2) = popStack stack1
          op = getOp x
      in helper xs $ pushStack (op operand1 operand2) stack2
   | otherwise = helper xs (pushStack (read x) stack)

input = "10 4 3 + 2 * -"

main = do
   let x = getRpnList input
   --mapM_ putStrLn x
   putStrLn $ show x


--fold version of code
solveRPN2 :: (Read a, Num a) => String -> a
solveRPN2 = head . foldl foldFunc [] . words
   where foldFunc (x:y:ys) "+" = (x*y):ys
         foldFunc (x:y:ys) "-" = (x+y):ys
         foldFunc (x:y:ys) "*" = (y-x):ys
         foldFunc xs numStr = read numStr:xs


--Heathrow to London

group3s [] = []
group3s (x:y:z:xs) = (x,y,z) : group3s xs


--[10, 90, 2, 8]
--[50, 30, 90, 2, 8]
--[10, 30, 5, 40, 10]

--([50], [10, 30])
--([10], [50, 30])

--([50, 5], [10, 30, 5], [10, 90, 20], [50, 30, 90, 20])
--([10, 90], [10, 30, 5, 20], [50, 30, 90], [50, 5, 20], [10, 30, 5, 20])



--getFastestPath :: [Int] -> [Int]
getFastestPath roadsLst =
   let roads = group3s roadsLst
       allRoutes = helperLH [[0]] [[0]] roads
   in (foldl (\x acc -> if sum x < sum acc then x else acc) [100000] allRoutes, allRoutes)


helperLH :: [[Int]] -> [[Int]] -> [(Int, Int, Int)] -> [[Int]]
helperLH left right [] = (map tail left) ++ (map tail right)
helperLH left right ((a, b, mid):xs) = helperLH newLeft newRight xs
   where zipped = zip left right
         newLeft  = concat $ map (\(l, r) -> [l ++ [a], r ++ [b, mid]]) zipped
         newRight = concat $ map (\(l, r) -> [r ++ [b], l ++ [a, mid]]) zipped

lhData1 = [50, 10, 30, 5, 90, 20]
lhData2 = [50, 10, 30, 5, 90, 20 , 40, 2, 25, 10, 8, 0]

{-|
50    40    10     50    60
===============================
    |     |      |     |
 10 |  50 |   20 |  10 |
    |     |      |     |
===============================
20    30     60    40    10
-}
lhData3 = [50, 20, 10, 40, 30, 50, 10, 60, 20, 50, 40, 10, 60, 10, 0]

{-|
50    40    10     50    60
===============================
    |     |      |     |
 20 |  20 |   20 |  20 |
    |     |      |     |
===============================
30    30     60    40    10
-}

lhData4 = [50, 30, 20, 40, 30, 20, 10, 60, 20, 50, 40, 20, 60, 10, 0]


{-|
11     5     10     5
=======================
    |     |      |
 2  |   4 |    6 |
    |     |      |
=======================
7      7      12    7
-}

lhData5 = [11, 7, 2, 5, 7, 4, 10, 12, 6, 5, 7, 0]



{-|
11     5     10     8
=======================
    |     |      |
 2  |   4 |    4 |
    |     |      |
=======================
7      7      6     6
-}

lhData6 = [11, 7, 2, 5, 7, 4, 10, 6, 4, 8, 6, 0]


(fastestPath, allPaths) = getFastestPath lhData6


