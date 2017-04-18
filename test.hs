import Data.List (tails, elemIndex, inits)

--putStr or putStrLn
v1 = "test \n foo"
v2 = "test \n boo"


split :: [Char] -> [[Char]]
split str = map (\x -> reverse x) (split' "" str) --split' currLine rest
   where split' currLine [] = [currLine]
         split' currLine [x] = [x:currLine]
         split' currLine ('\n':xs) = currLine : split' "" xs
         split' currLine (x:xs) = split' (x:currLine) xs


--splitStr




---- ## Recursion

take' :: (Num i, Ord i) => i -> [a] -> [a]
take' n _
   | n <= 0 = []
take' _ [] = []
take' n (x:xs) = x : take' (n-1) xs

elem' :: (Eq a) => a -> [a] -> Bool
elem' _ [] = False
elem' e (x:xs)
   | x == e       = True
   | otherwise    = elem' e xs

quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) = lesser ++ [x] ++ greater
   where lesser = quicksort [y | y <- xs, y < x]
         greater = quicksort [y | y <- xs, y >= x]


---- ## Higher-Order functions

applyTwice :: (a -> a) -> a -> a
applyTwice f x = f $ f x

--ex 4 (zipWith):
zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ [] _ = []
zipWith' _ _ [] = []
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys

flip' :: (a -> b -> c) -> (b -> a -> c)
flip' f = f'
   where f' a b = f b a


--Collatz sequences

chain :: (Integral a) => a -> [a]
chain 1 = [1]
chain x
   | x == 1 = [1]
   | even x = x : chain (x `div` 2)
   | odd x  = x : chain (x*3 + 1)


goodChainz1 = length $ filter (\x -> length x > 15) (map chain [1..100])
goodChainz2 = length $ filter isLong $ map chain [1..100]
   where isLong x = length x > 15


-- ## fold:

--foldl
foldl' :: (b -> a -> b) -> b -> [a] -> b
foldl' _ start [] = start
foldl' f start (x:xs) = foldl' f (f start x) xs


--foldr
foldr' :: (a -> b -> b) -> b -> [a] -> b
foldr' _ start [] = start
foldr' f start (x:xs) = f x (foldr' f start xs)


--ex1 (sum):
sum' :: (Num a) => [a] -> a
sum' x = foldl' (+) 0 x


--ex2 (prod)
prod :: (Num a) => [a] -> a
prod xs = foldr' (*) 1 xs


--ex3 (map foldr):
map1 :: (a -> b) -> [a] -> [b]
map1 f xs = foldr' (\x acc -> f x : acc) [] xs


--ex4 (map foldl)
map2 :: (a -> b) -> [a] -> [b]
map2 f xs = foldl' (\acc x -> acc ++ [f x]) [] xs


foldr1' :: (a -> a -> a) -> [a] -> a
foldr1' _ [] = error "not enough arguments"
foldr1' f [x] = x
foldr1' f (x:xs) = f x (foldr1' f xs)

foldl1' :: (a -> a -> a) -> [a] -> a
foldl1' _ [] = error "need at least 1 arg"
foldl1' f (x:xs) = foldl f x xs




---- ## Modules

-- # inits/tails

--search for sublist (same as isInfixOf)
search :: (Eq a) => [a] -> [a] -> Bool
search needle haystack =
   let nlen = length needle
   in foldl (\acc x -> if take nlen x == needle
               then True else acc)
            False
            (tails haystack)



-- ## custom dictionary

phoneBook =
   [ ("bob", "555-5432")
   , ("michael", "235-403-2813")
   , ("brian", "321-345-5683")
   ]

--throws
findKey1 :: (Eq a) => [(a, b)] -> a -> b
findKey1 dict key = snd . head . filter (\(a, b) -> a == key) $ dict


--good maybe version
findKey2 :: (Eq a) => [(a, b)] -> a -> Maybe b
findKey2 [] _ = Nothing
findKey2 ((k, v):xs) key = if key == k
   then Just v
   else findKey2 xs key


--best: using foldr reverse key/dict args
findKey3 :: (Eq a) => a -> [(a, b)] -> Maybe b
findKey3 key dict = foldr
   (\(k,v) acc -> if k==key then Just v else acc)
   Nothing
   dict
--or
findKey3' key = foldr
   (\(k,v) acc -> if k==key then Just v else acc)
   Nothing



---- #### typeclasses/ADT

--binary search tree

data Tree a = EmptyTree | Node a (Tree a) (Tree a)
              deriving (Show, Read, Eq)

singleton :: a -> Tree a
singleton x = Node x EmptyTree EmptyTree

treeInsert :: (Ord a) => a -> Tree a -> Tree a
treeInsert x EmptyTree = singleton x
treeInsert x (Node a left right)
   | x == a = Node x left right
   | x < a = Node a (treeInsert x left) right
   | x > a = Node a left (treeInsert x right)

treeFromList :: (Ord a) => [a] -> Tree a
treeFromList = treeFromList'

treeFromList' :: (Ord a) => Tree a -> [a] -> Tree a
treeFromList' tree [] = tree
treeFromList' [x] = singleton x
treeFromList' (x:xs)


treeElem :: (Ord a) => a -> Tree a -> Bool
treeElem _ EmptyTree = False
treeElem x (Node a left right)
   | x == a = True
   | x < a  = treeElem x left
   | x > a  = treeElem x right



---- #### 500 datastructures

-- https://www.reddit.com/r/programming/comments/65njzp/500_data_structures_and_algorithms_interview/

-- ## Ex 1:
--http://www.techiedelight.com/find-pair-with-given-sum-array/

--v1:
findPair1 :: (Num a, Eq a) => a -> [a] -> Maybe (a, a, Int, Int)

findPair1 _ [] = Nothing
findPair1 _ [_] = Nothing
findPair1 sum (x:xs) = if elem (sum - x) xs
   then Just (x, (sum-x), 0, 0)
   else findPair1 sum xs


--v2:
elimMaybe (Just x) = x

findPair2' :: (Num a, Eq a) => a -> [a] -> Int -> Maybe (a, a, Int, Int)
findPair2' _ [] _ = Nothing
findPair2' _ [_] _ = Nothing
findPair2' sum lst@(x:xs) index1 =
   let x2 = (sum-x)
       isElem = elemIndex x2 xs
   in case isElem of
         Nothing -> findPair2' sum xs (index1+1)
         Just i2 -> Just (x, (sum-x), index1, index1+i2+1)

findPair2 :: (Num a, Eq a) => [a] -> a -> Maybe (a, a, Int, Int)
findPair2 xs sum = findPair2' sum xs 0

printResFindPair :: (Show a) => Maybe (a, a, Int, Int) -> IO ()
printResFindPair Nothing = putStrLn "Error"
printResFindPair (Just (x1, x2, i1, i2)) =
   putStrLn $ "Pair Found at Index " ++ (show i1) ++ " and "
               ++ (show i2) ++ " (" ++ (show x1)
               ++ " + " ++ (show x2) ++ ")"

--ghci test:
--printResFindPair $ findPair2 [8, 7, 2, 5, 3, 1] 10



-- ## Ex 2:
--http://www.techiedelight.com/find-sub-array-with-0-sum/

--print all sub-array with 0 sum
search' :: (Eq a, Num a) => ([a] -> Bool) -> [a] -> [[a]]
search' pred xs =
   foldl (\ret sub ->
            let subs = map (\a -> take a sub) [1..length sub]
                good = foldl (\acc x -> if pred x then acc++[x] else acc)
                             []
                             subs
            in ret ++ good)

         []
         (tails xs)

subs0Sum xs = filter (not . null) $ search' (\x -> sum x == 0) xs
--subs0Sum [4, 2, -3, -1, 0, 4]





