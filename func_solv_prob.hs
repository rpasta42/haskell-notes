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
solveRPN2 str = head . foldl foldFunc [] . words
   where foldFunc (x:y:ys) "+" = (x*y):ys
         foldFunc (x:y:ys) "-" = (x+y):ys
         foldFunc (x:y:ys) "*" = (y-x):ys
         foldFunc xs numStr = read numStr:xs


--Heathrow to London

group3s [] = []
group3s (x:y:z:xs) = (x,y,z) : group3s xs


getFastestPath :: [Int] -> [Int]
getFastestPath = group3s


