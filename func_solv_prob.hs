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


newStack = []

pushStack :: a -> [a] -> [a]
pushStack x xs = x:xs

popStack :: [a] -> (a, [a])
popStack (x:xs) = (x, xs)

getRpnList = words --filter ((/=) " ") $ words --partition ((==) ' ') str -- group str
isOp x = or $ map (==x) ["+", "-", "*"]

getOp "+" = (+)
getOp "-" = (-)
getOp "*" = (*)

strToInt s = (read s) :: Int

--solveOp :: (Num a, Read a) => (a -> a -> a) -> String -> String -> a
solveOp op a b =
   let numA = a-- (read a) --strToInt a
       numB = b--(read b) --strToInt b
   in op numA numB

solveRPN :: (Read a, Num a) => String -> a
solveRPN str = helper (getRpnList str) newStack

helper :: (Read a, Num a) => [String] -> [a] -> a
helper [] stack = fst . popStack $ stack
helper (x:xs) stack
   | isOp x =
      let (operand1, stack1) = popStack stack
          (operand2, stack2) = popStack stack1
          op = getOp x
      in helper xs $ pushStack (solveOp op operand1 operand2) stack2
   | otherwise = helper xs (pushStack (read x) stack)

input = "10 4 3 + 2 * -"

main = do
   let x = getRpnList input
   --mapM_ putStrLn x
   putStrLn $ show x

