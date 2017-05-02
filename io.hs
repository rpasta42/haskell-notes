---- #### Big section
---- ### Sub-section
-- ## Big separator
-- # Separator
-- normal comment

import Data.Char
import Control.Monad
import System.IO
import System.IO.Error
import Control.Exception
import System.Environment
import System.Random
import System.Environment
import qualified Data.ByteString.Lazy as B

---- #### IO

-- ## loading/compiling examples:

-- # helloworld.hs file content
main1 = putStrLn "Hello, world" --replace main1 with main


-- # loading in ghci and running
-- > :l helloworld
-- > main


-- # making executable
-- $ ghc --make helloworld
-- $ ./helloworld


-- # compiling/running as one step
-- $ runhaskell helloworld.hs



-- ## main and IO/funcs

--ex 0 (putStrLn):
-- :t putStrLn
   -- putStrLn :: String -> IO ()
main_ex0 = putStrLn "test"


--setup for ex1/2:
addGreetingToName name = "Hey " ++ name ++ ", you rock"

--ex 1:
main_ex1 = do
   putStrLn "Hello, what's your name"
   name <- getLine
   putStrLn (addGreetingToName name)


--ex 2:
main_ex2 = do
   foo <- putStrLn "Hello, what's your name" --foo has ()
   name <- getLine
   putStrLn (addGreetingToName name)


--ex 3 (let in do):

main_ex3 = do
   putStrLn "First name?"
   firstName <- getLine
   putStrLn "Last name?"
   lastName <- getLine

   let bigFirstName = map toUpper firstName
       bigLastName = map toUpper lastName
   putStrLn $ "Hi " ++ bigFirstName ++ " " ++ bigLastName ++ ", how r u?"


--ex 4 (reverse words loop with return):
--terminates on empty line

reverseWords :: String -> String
reverseWords = unwords . map reverse . words

main_ex4 = do
   line <- getLine
   if null line
      then return ()
      else do
         putStrLn $ reverseWords line
         main_ex4

--ex 5 (return and name binding):
main_ex5 = do
    c <- getChar
    if c /= ' '
        then do
            putChar c
            main_ex5
        else return ()


--ex 6 (when):
main_ex6 = do
   c <- getChar
   when (c /= ' ') $ do
      putChar c
      main_ex6

--ex 7 (sequence):

--v1:
main_ex7_v1 = do
   a <- getLine
   b <- getLine
   c <- getLine
   print [a, b, c]

--equivelent v2:
main_ex7_v2 = do
   rs <- sequence [getLine, getLine, getLine]
   print rs


--ex 8 (another sequence example):
main_ex8 = sequence $ map print [1,2,3]


--ex 9 (mapM/mapM_ same as sequence):
main_ex9 = mapM print [1,2,3]


--ex 10 (forever):

--import Control.Monad
--import Data.Char

main_ex10 = forever $ do
   putStr "Give me some input: "
   l <- getLine
   putStrLn $ map toUpper l


--ex 11 (forM):
main_ex11 = do
   colors <- forM [1,2,3,4] (\a -> do
      putStrLn $ "Which color u associate w/ # " ++ show a ++ "?"
      color <- getLine
      return color)
   putStrLn "The colors associated w/ nums 1, 2, 3 & 4 are: "
   mapM putStrLn colors

--instead of doing color <- getLine and then return
--color, we could just do "getLine" on the last line


-- ## Streams

-- ex12 (getContents):

--import Data.Char

main_ex12 = do
   contents <- getContents
   putStr (map toUpper contents)

--getContent isn't a real string, but more like a promise


--used in ex 13 and ex 14
shortLinesOnly :: String -> String
shortLinesOnly xs =
   let xsLines = lines xs
       xsShortOnly = filter (\line->length line < 10) xsLines
   in  unlines xsShortOnly

--ex 13 (getContents):
main_ex13 = do
   contents <- getContents
   --putStr . shortLinesOnly $ contents
   putStr $ shortLinesOnly contents


--ex 14 (interact & getContents):
main_ex14_v1 = interact shortLinesOnly
--or
main_ex14_v2 = interact $ unlines . filter ((<10) . length) . lines


--ex 15 (palindrome):

--reads line, prints (not) palindrome for each one
main_ex15 = interact (unlines . map (\str ->
   if str == reverse str then "plaindrome" else "not pal") . lines)



-- ## Files

--ex 16 (reading files):

--import System.IO
main_ex16 = do
   handle <- openFile "io.hs" ReadMode
   contents <- hGetContents handle
   putStr contents
   hClose handle

--ex 17 (withFile):

--import System.IO
main_ex17 = do
   withFile "io.hs" ReadMode (\handle -> do
      contents <- hGetContents handle
      putStr contents)

--ex 18 (readFile):
main_ex18 = do
   contents <- readFile "io.hs"
   putStr contents


--ex 19 (hSetBuffering):
main_ex19 = do
   withFile "io.hs" ReadMode (\handle -> do
      hSetBuffering handle $ BlockBuffering (Just 2048)
      contents <- hGetContents handle
      putStr contents)



-- ## cmd args

--ex 20 (cmd args):
main_ex20 = do
   args <- getArgs
   progName <- getProgName
   mapM putStrLn args
   putStrLn progName
   x <- getLine
   putStrLn x

-- ## random

--ex 21 (random coins):
threeCoins :: StdGen -> (Bool, Bool, Bool)
threeCoins gen =
   let (coin1, newGen) = random gen
       (coin2, newGen') = random newGen
       (coin3, _) = random newGen'
   in (coin1, coin2, coin3)

countTrueCoins :: [Int] -> (Int, Int, Int)
--countTrue stdRange
countTrueCoins xs =
   let allCoins = map (\i -> threeCoins $ mkStdGen i) xs
       trueTo1 True = 1
       trueTo1 False = 0
   in foldr (\(c1, c2, c3) (acc1, acc2, acc3) -> (acc1+trueTo1 c1, acc2+trueTo1 c2, acc3+trueTo1 c3))
            ((0, 0, 0) :: (Int, Int, Int))
            allCoins


main_ex21 = do let coins = threeCoins (mkStdGen 100)
               print coins
               return 0


--ex 22 (getStdGen with I/O):

main_ex22 = do
   randGen <- getStdGen
   putStr $ take 20 (randomRs ('a','z') randGen)



-- ## bytestrings

--ex 23 (copying files with bytestrings):

main_ex23 = do
   (fileName1:fileName2:_) <- getArgs
   copyFile fileName1 fileName2

copyFile :: FilePath -> FilePath -> IO ()
copyFile source dest = do
   contents <- B.readFile source
   B.writeFile dest contents

--usage runhaskell io.hs something.txt ../../something.txt



-- ## Exceptions

--ex 24/25 (IO exception handling)

main_ex24 = toTry `catch` handler1
main_ex25 = toTry `catch` handler2

toTry :: IO ()
toTry = do (fileName:_) <- getArgs
           contents <- readFile fileName
           putStrLn $ "The file has " ++ show (length $ lines contents) ++ " lines!"

handler1 :: IOError -> IO ()
handler1 e = putStrLn "Whoops, had some trouble"

handler2 :: IOError -> IO ()
handler2 e
      | isDoesNotExistError e = (do
         putStrLn "The file doesn't exist!"
         (case ioeGetFileName e of
            Just path -> putStrLn $ "file name " ++ path
            Nothing -> putStrLn "Nothing"))
      | isFullError e = putStrLn "full error"
      | isIllegalOperation e = putStrLn "illegal"
      | otherwise = ioError e


--kk


main = main_ex22



