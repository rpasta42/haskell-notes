---- #### Big section
---- ### Sub-section
-- ## Big separator
-- # Separator
-- normal comment


---- #### Making your own types

---- ### Algebraic data types

-- ## Basics

-- # Making ADT's

data Bool = False | True

data Shape = Circle Float Float Float | Rectangle Float Float Float Float

--:t Circle
Circle :: Float -> Float -> Float -> Shape


-- # Using ADT's

-- # Pattern-matching
surface :: Shape -> Float
surface (Circle _ _ r) = pi * r ^ 2
surface (Rectangle x1 y1 x2 y2) = (abs $ x2 - x1) *
                                  (abs $ y2 - y1)

-- usage:
surface $ Circle 10 20 10
--314.15927
surface $ Rectangle 0 0 100 100
--10000


-- # using ADT with typeclass

data Shape = Circle Float Float deriving (Show)

--ghci> Circle 10 20
--displays: Circle 10.0 20.0

map (Circle 10) [4, 5, 6]
--output: [Circle 10.0 4, Circle 10 5, Circle 10 6]


-- # ADT's with ADT's

data Point = Point Float Float deriving (Show)
data Shape = Circle Point Float | Rectangle Point Point deriving (Show)

surface :: Shape -> Float
surface (Circle _ r) = pi * r ^ 2
surface (Rectangle (Point x1 y1) (Point x2 y2)) = (abs $ x2 - x1) * (abs $ y2 - y1)




-- ## Record syntax


--bad:
data Person = Person String String Int Float String String deriving (Show)

--good:
data Person = Person { firstName :: String
                     , lastName :: String
                     , age :: Int
                     , height :: Float
                     , phoneNumber :: String
                     , flavor :: String
                     } deriving (Show)

--automatically makes functions for getters
firstName :: Person -> String

--usage:

Person {firstName="Test", lastName="Blah", age=35, height=154, phoneNumber="304-32-32", flavor="test"}
--can still construct using old method

-- # pattern matching on Records

--can leave out type arguments
tellPerson (Person {firstName=f, lastName=l}) = f ++ l



-- ## Type parameters

-- # ex1:
data Maybe a = Nothing | Just a

-- # ex2:
data Either a b = Left a | Right b deriving (Eq, Ord, Read, Show)

--:t Right 'a'
--Right 'a' :: Either a Char


-- # to view types:
--:k Maybe


-- # Data with typeclasses

--data (Ord k) => Map k v = ...
toList :: Map k a -> [(k, a)]

--bad practice to add typeclass constraints in data declaration (put in functions instead)

--can write like this better
toList :: (Ord k) => Map k a -> [(k, a)]

-- vector example ex:

data Vector a = vector a a a deriving (Show)

vplus :: (Num t) => Vector t -> Vector t -> Vector t
(Vector i j k) `vplus` (vector k m n) = Vector (i+l) (j+m) (k+n)



-- ## Derived instances

data Person = Person { firstName :: String
                     , lastName :: String
                     } deriving (Eq)


data Day = Monday|Tuesday|Wednesday|Thursday|Friday|Saturday|Sunday
           deriving (Eq, Ord, Show, Read, Bounded, Enum)

show Wednesday -- returns String
read "Saturday" :: Day --opposite

[minBound .. maxBound] :: [Day] --lists all days



---- ### Type synonyms

-- ## Basic usage

--ex 1:
type String = [Char]

--ex 2:
type Phonebook = [(String, String)]


-- ## Parameterized
type AssocList k v = [(k, v)]

--lookup type signature
(Eq k) => k -> AssocList k v -> Maybe v



---- ### Recursive data structures

--ex1 (list):
data List a = Empty | Cons a (List a) deriving (Show, Read, Eq, Ord)

--ex2 (list with infix cons operator):
infixr 5 :-:
data List a = Empty | a :-: (List a) deriving (Show, Read, Eq, Ord)

--usage
3 :-: 4 :-: 5 :-: Empty

--ex3 (list addition)
infixr 5 .++
(.++) :: List a -> List a -> List a
Empty .++ ys = ys
(x :-: xs) .++ ys = x :-: (xs .++ ys)


---- ## Binary search tree
--see test.hs


---- ### Making typeclasses

-- # ex 1:

class Eq a where
   (==) :: a -> a -> Bool
   (/=) :: a -> a -> Bool
   x == y = not (x /= y)
   x /= y = not (x == y)

-- # ex 2:

data TrafficLight = Red | Yellow | Green

instance Eq TrafficLight where
   Red == Red = True
   Green == Green = True
   Yellow == Yellow = True
   _ == _ = False

instance Show TrafficLight where
   show Red = "Red light"
   show Yellow = "Yellow light"
   show Green = "Green light"







