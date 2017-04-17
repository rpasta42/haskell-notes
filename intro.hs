

---- ## Intro

-- ## comments

-- comment

-- ## math operations
2+15
48*100
32-3
5/2
--no 5 * -3
5 * (-3)

--modulus
3 `mod` 8 --3
mod 5 3 --2

-- ## boolean algebra
True && False
False || True
not False

-- ## comparison

5 == 10
5 /= 10
"test" == "test"
4 > 10
4 < 3

-- ## variable

x = 3
-- ghci variable
let x = 3

-- ## function
f x = 3+x

-- ## function invocation
f 3

-- ## if/else
f x = if x > 100
      then x
      else x*2

-- ## build-ins
succ 8
min 9 10
max 11 5

(succ 9) + (max 5 4) + 1

-- ## list

x = [1, 2, 3]

--addition
[1, 2, 3] ++ [4, 5, 6]
"hello" ++ "world"

--cons operator
5:[1,2,3]
--index operator
[1, 2, 3] !! 0 --returns 1
[1, 2, 3] !! 1 --returns 2


--comparison lexicographical order
[3, 2, 1] > [2, 1, 0] -- True
[3, 4, 2] == [3, 4, 2] -- True

-- head/tail/last/init (head/tail = car/cdr)
head [5, 4, 3] -- returns 5
tail [5, 4, 3] -- returns [4, 3]
last [5, 4, 3] -- returns 3
init [5, 4, 3] -- returns [5, 4]

head [] -- exception

--length/null
length [] -- 0
length [1, 2] -- 2
null [1, 2, 3] -- False
null [] -- True

-- reverse, take, drop
reverse [5, 4, 3] -- [3, 4, 5]

take 5 [1, 2] -- [1, 2]
take 2 [1, 2, 3, 4] -- [1, 2]
take 0 [1, 2] -- []

drop 3 [1, 2, 3, 4, 5] -- [4, 5]
drop 100 [1, 2, 3] -- []

-- maximum/minimum
maximum [1, 2, 3] -- 3
minimum [1, 2, 3] -- 1

-- sum/product
sum [1, 2, 3] -- 6
product [1, 2, 3] -- 5

-- elem
elem 4 [3, 4, 5] -- True
4 `elem` [3, 4, 5] -- True
2 `elem` [3, 4, 5] -- False

-- cycle/repeat/replicate
take 3 (cycle [1, 2]) -- [1, 2, 1, 2, 1, 2]
take 12 (cycle "LOL ") -- "LOL LOL LOL "

take 3 $ repeat 5 --[5, 5, 5]

replicate 3 10 -- [10, 10, 10]

-- ## texas range
[1..3] -- [1, 2, 3]
['a'..'z'] -- alphabet
['A'..'Z'] -- uppercase alphabet

[2, 4 .. 20] -- [2, 4, 6, 8, ...]

--floating point ranges funky
[0.0, 0.2 .. 1] -- [0.0, 0.2, 0.4, 0.600001, 0.80002, 1.000]

--combining
take 24 [13, 26..]


-- ## list comprehension

[x*2 | x <- [1..4]] --[2, 4, 6, 8]

[x*2 | x <- [1..10], x*2 >= 12] --[12, 14, 16, 18, 20]

boomBang xs = [ if x < 10 then "BOOm!" else "BANG" | x <-xs, odd x]
boomBang [7..13] -- ["BOOM", "BOOM", "BANG", "BANG"]

--multiple predicates
[x*y | x <- [3..6], x /= 4, x /=5, y <- [2, 3]] -- [6, 9, 12, 18]

let nouns = ["hobo", "pope"]
let adjectives = ["lazy", "scheming"]
[adjective ++ " " ++ noun | adjective <- adjectives, noun <- nouns]
   --out: ["lazy hobo", "lazy pope", "scheming hobo", "scheming pope"]

-- ## names that will never be used, use _
--custom length function
length' xs = sum [1 | _ <- xs]

-- ## tuples

--fst/snd
fst (8, 11) -- 8
snd (16, "Test") -- "Test"

-- ## zip

zip [1, 2, 3] ["test", "blah", "foo", "bar"] -- [(1, "test"), (2, "blah"), (3, "foo")]


---- ## Types

removeUpper :: [Char] -> [Char]
removeUpper xs = [x | x <- xs, not $ x `elem` ['A'..'Z']]

add3Nums :: Int -> Int -> Int -> Int
add3Nums x y z = x+y+z

-- long precision
factorial :: Integer -> Integer
factorial n = product [1..n]

-- ## typeclasses


--show
show 3 -- "3"
show True -- "True"

---- ## Syntax in Functions

-- ## Pattern matching
--ex1:
lucky :: (Integral a) => a -> String
lucky 7 = "You picked lucky number"
lucky x = "Bad number"

--ex2:
factorial :: (Integral a) => a -> a
factorial 0 = 1
factorial n = n * factorial (n - 1)
factorial n = n * (factorial $ n - 1)
factorial n = (*) n $ factorial $ n - 1

--ex 3:
--old
addVector :: (Num a) => (a, a) -> (a, a) -> (a, a)
addVectors a b = (fst a + fst b, snd a + snd b)

--new with pattern matching:
addVectors (x1, y1) (x2, y2) = (x1+x2, y1 + y2)

--ex 3 (list comprehension):
let xs = [(1, 3), (4, 3), (2, 4)]
[a+b | (a, b) <- xs]

--ex 4:
head' (x:_) = x
head' [] = error "hi"

--ex 5:
test [] = "Empty List"
test (x:[]) = "list with 1 element"
test (x:y:[]) = "list with 2 elements"
test (x:y:_) = "long list"

--ex 6:
capital :: String -> String
capital "" = "Empty string"
capital all@(x:xs) = "Whole string: " ++ all ++ "first leter" ++ [x]


-- ## Guards/where clause

--ex1:
bmiTest :: (RealFloat a) => a -> a -> String
bmiTest weight height --bmi
   | bmi <= skinny = "You're underweight"
   | bmi <= normal = "You're normal"
   | bmi <= 30.0 = "You're above normal"
   | otherwise   = "Way above normal"
   where bmi = weight / height ^ 2
         (skinny, normal) = (18.5, 25.0)

--ex2 (inline guard):
max' :: (Ord a) => a -> a -> a
max' a b | a > b = a | otherwise b

--ex3 (where):
initials firstname lastname = [f] ++ ". " ++ [l] ++ "."
   where (f:_) = firstname
         (l:_) = lastname

--ex4 (where):
calcBmis :: (RealFloat a) => [(a, a)] -> [a]
calcBmis xs = [bmi w h | (w, h) <- xs]
   where bmi weight height = weight / height ^ 2

-- ## let

cylinder :: (RealFloat a) => a -> a -> a
cylinder r h =
   let sideArea = 2 * pi * r * h
       topArea = pi * r ^ 2
   in  sideArea + 2 * topArea

--inline
4 * (let a = 9; b = 92 in a+b+1) + 2
(let (a, b, c) = (1, 2, 3) in a+b+c) * 100 -- 600

--inside list comprehension
calcBmisFat :: (RealFloat a) => [(a, a)] -> [a]
calcBmisFat xs = [bmi | (w, h) <- xs, let bmi = w / h ^ 2, bmi >= 25.0]

-- ## case

--ex 1:
head' :: [a] -> a
head' [] = error "Empty"
head' (x:_) = x

--same as
head' :: [a] -> a
head' xs = case xs of [] -> error "Empty"
                      (x:_) -> x

--ex2:
testList xs = "List is " ++ case xs of [] -> "empty."
                                       [x] -> "single item."
                                       xs -> "a long list"

--ex3 with where:
testList xs = "The list is " ++ what xs
   where what [] = "empty."
         what [x] = "a singleton list."
         what xs = "a longer list."

--ex4 inline:
case 2 of { (1) -> "A"; (2) -> "B"; (3) -> "C" }


---- ## Recursion

--ex1 maximum:
maximum' :: (Ord a) => [a] ->
maximum' [] = error "maximum of empty"
maximum' [x] = x
maximum' (x:xs)
   | x > maxTail = x
   | otherwise = maxTail
   where maxTail = maximum' xs

--ex2 inline max:
let maximum' xs = case xs of {([]) -> error "Empty"; ([x]) -> x; (x:xs) -> (let maxTail = maximum' xs in if x > maxTail then x else maxTail)}

--ex3 repeat:
repeat' :: a -> [a]
repeat' x = x:repeat' x

--ex4 zip:
zip' :: [a] -> [b] -> [(a, b)]
zip' _  [] = []
zip' [] _  = []
zip (x:xs) (y:ys) = (x, y):zip' xs ys

--ex5 elem:
elem' :: (Eq a) => a -> [a] -> Bool
elem' _ [] = False
elem' e (x:xs)
   | x == e       = True
   | otherwise    = elem' e xs

--ex6 quicksort:
quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) = lesser ++ [x] ++ greater
   where lesser = quicksort [y | y <- xs, y < x]
         greater = quicksort [y | y <- xs, y >= x]



---- ## Higher-Order functions

-- ## currying

--ex1:
max 4 5 -- return: 5
max 4 -- returns: (max 4) :: (Num a, Ord a) => a -> a
(max 4) 5 -- returns: 5

--ex2:
multThree :: (Num a) => a -> a -> a -> a
--or
multThree :: (Num a) => a -> (a -> (a -> a))

multThree x y z = x * y * z
let multTwoNumsWithNine = multThree 9

--ex3:
compareWith100 :: (Num a, Ord a) => a -> Ordering
comparewith100 = compare 100

divideBy10 :: (Floating a) => a -> a
divideBy10 = (/10)

isUpper :: Char -> Bool
isUpper = (`elem` ['A'..'Z'])

--ex 4 (function parameters):
applyTwice :: (a -> a) -> a -> a
applyTwice f x = f $ f x

applyTwice (+3) 2 --returns: 8

--ex 4 (zipWith):
zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ [] _ = []
zipWith' _ _ [] = []
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys


zipWith' (\x y -> x+y) [1, 2, 3] [3, 4]
--Returns: [4, 6]
--same as:
zipWith (+) [1, 2, 3] [3, 4]

zipWith' (*) (replicate 5 2) [1..]

--ex 5 (flip):
flip' :: (a -> b -> c) -> (b -> a -> c)
flip' f = f'
   where f' a b = f b a
--or
flip' :: (a -> b -> c) -> (b -> a -> c)
flip' f y x = f x y
--or
flip' f = \x y -> f y x
--or
flip' = \f x y -> f y x


-- ## maps & filters

--ex1 (map):
map' :: (a -> b) -> [a] -> [b]
map' _ [] = []
map' f (x:xs) = (f x) : map' f xs

--ex2 (filter):
filter' :: (a -> Bool) -> [a] -> [a]
filter' _ [] = []
filter' f (x:xs)
   | f x       = x : filter' f xs
   | otherwise = filter' f xs

filter (>3) [1, 2, 5, 6]
filter even [1..10]


-- ## takewhile

--ex1:
sum (takeWhile (<1000) (filter odd (map (^2) [1..])))
--or:
sum $ takeWhile (<1000) $ filter odd (map (^2) [1..])

-- ## Lambdas

x = \a b -> a + b

--can pattern match, but only 1 pattern
--ex1:
map (\(a, b) -> a + b) [(1, 2), (3, 5), (6, 10)]

--ex2:
addThree :: (Num a) => a -> a -> a -> a
addThree x y z = x+y+z
--or
addThree = \x -> \y -> \z -> x + y + z

-- ## Folds (foldl, foldr, foldl1, foldr1)

-- foldr' and foldr1' can crash the stack
-- foldl is stricter

--foldl
foldl' :: (b -> a -> b) -> b -> [a] -> b
foldl' _ start [] = start
foldl' f start (x:xs) = foldl' f (f start x) xs

--foldr
foldr' :: (a -> b -> b) -> b -> [a] -> b
foldr' _ start [] = start
foldr' f start (x:xs) = f x (foldr' f start xs)

-- look at code files for basic fold examples

-- ## foldl1 foldr1

--same as folds, but no startin value
sum = foldl1 (+)

maximum' :: (Ord a) => [a] -> a
maximum' = foldr1 (\x acc -> if x > acc then x else acc)

reverse' :: [a] -> [a]
reverse' = foldl (\acc x -> x : acc) []

reverse' :: [a] -> [a]
--or
reverse' = foldl (flip (:)) []

product' :: (Num a) => [a] -> a
product' = foldr1 (*)

filter' :: (a -> Bool) -> [a] -> [a]
filter' p = foldr (\x acc -> if p x then x : acc else acc) []

head' :: [a] -> a
head' = foldr1 (\x _ -> x)

last' :: [a] -> a
last' = foldl1 (\_ x -> x)


-- ## Scans scanl scanr, scanl1, scanr1

--like folds, but return intermediary accumulators as lists

--ex1:
scanl (+) 0 [3, 5, 2, 1]
   --returns: [0, 3, 8, 10, 11]

--ex2:
scanr (+) 0 [3, 5, 2, 1]
   --returns: [11, 8, 3, 1, 0]

--ex3:
--how many elements does it take for the sum of
--the roots of all natural numbers to exceed 1000

sqrtSums :: Int
sqrtSums = 1 + length (takeWhile (<1000) $ scanl1 (+) (map sqrt [1..]))


-- ## function application (with $)

($) :: (a -> b) -> a -> b
f $ x = f x

--ex1:
sum $ map sqrt [1..130]

--ex2:
--function application can be treated just like another function
[(4+) $ 3, (10*) $ 3]

map ($ 3) [(4+), (10*)]

-- ## function composition (with .)
(.) :: (b -> c) -> (a -> b) -> a -> c
f . g = \x -> f (g x)

-- ex1:
(negate . (*3)) 5
--or
negate . (*3) $ 5


-- ex2:
map (\x -> 3 + negate (abs x)) [5, -3, 8, 24]
--or
map ((+3) . negate . abs) [5, -3, 8, 24]
   --returns [-2, 0, -5, -21]


-- ex3 (partial application):

--bad:
sum (replicate 5 (max 6.8 8.9))
--good:
sum . replicate 5 . max 6.8 $ 8.9
--or
(sum . replicate 5 . max 6.8) 8.9
--or
sum . replicate 5 $ max 6.8 8.9


--ex4:

oddSquareSum :: Integer
--bad:
oddSquareSum = sum (takeWhile (<10000) (filter odd (map (^2) [1..])))
--good:
oddSquareSum = sum . takeWhile (<10000) . filter odd . map (^2) $ [1..]
--better:
oddSquareSum =
   let oddSquares = filter od $ map (^2) [1..]
       belowLimit = takeWhile (<10000) oddSquares
   in  sum belowLimit




---- ## Modules

--ex1:
import Data.List

numUniques :: (Eq a) => [a] -> Int
numUniques = length . nub

--ex2 (put module functions in global namespace in ghci):
:m + Data.List

--ex3 (put multiples modules into global namespace):
:m + Data.List Data.Map Data.Set

-- ## Data.List

-- # transpose

--ex1:
-- 3x^2 + 5x + 9
-- 10x^3 + 9
-- 8x^3 + 5x^2 + x - 1

--represent in haskell:
--[[0, 3, 5, 9], [10, 0, 0, 9], [8, 5, 1, -1]]

--too add them:
map sum $ transpose [[0, 3, 5, 9], [10, 0, 0, 9], [8, 5, 1, -1]]
--returns [18, 8, 6, 17]

-- # takeWhile

--ex1:
takeWhile (/=' ') "This is a sentence"
--returns: "This"

--ex2:
--we want to know the sum of all third powers that are under $10,000

--this will never return
filter (<10000) . map (^3) $ [1..]

--so we do this instead:
sum $ takeWhile (<10000) $ map (^3) [1..]

-- # inits/tails

--ex1:
--search for sublist (same as isInfixOf)
search :: (Eq a) => [a] -> [a] -> Bool
search needle haystack =
   let nlen = length needle
   in foldl (\acc x -> if take nlen x == needle
               then True else acc)
            False
            (tails haystack)



-- ## making your own mudles

-- # module creation ex 1

----Geometry.hs
module Geometry
( sphereVolume
, sphereArea
) where

--or
module Geometry
( * ) where

sphereVolume :: Float -> Float
sphereVolume radius = (4.0 / 3.0) * pi * (radius ^ 3)

sphereArea :: Float -> Float
sphereArea radius = 4 * pi * (radius ^ 2)

--not exported
helper blah = 3


----to use module
import Geometry


-- # module creation ex 2

-- #hierarchical structure
--1.) make folder Geometry
--2.) Geometry has 2 files:
--    Sphere.hs, Cube.hs, Cuboid.hs
--3.) Cube.hs uses Cuboid.hs stuff

----Sphere.hs:
module Geometry.Sphere
( volume
, area
) where

--define volume volume ...
--define area ...

----Cuboid.hs
module Geometry.Cuboid
( volume
, area
) where

volume :: Float -> Float -> Float -> Float
volume a b c = rectangleArea a b * c

area :: Float -> Float -> Float -> Float
area a b c = rectangleArea a b * 2 + rectangleArea
a c * 2 + rectangleArea c b * 2

rectangleArea :: Float -> Float -> Float
rectangleArea a b = a * b

----Cube.hs
module Geometry.Cube
( volume
, area
) where

import qualified Geometry.Cuboid as Cuboid

volume :: Float -> Float
volume side = Cuboid.volume side side side

area side = Cuboid.area side side side







