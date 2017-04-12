import Data.List (tails)

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





