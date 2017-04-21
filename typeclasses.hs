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

-- :t Circle
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

-- typeclass instances need to have concrete type, not type constructors
-- typeclasses can take type constructors (like functor)


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


-- ## Typeclass subclass of other typeclass

-- # ex 1 Number needs to be Eq first:

class (Eq a) => Num a where
   ...


-- # ex 2 (Maybe Eq):

--can't have function a -> Maybe, but can a -> Maybe a
--this doesn't work
instance Eq Maybe where
   ...

--m needs to be eq too, so this is also bad
instance Eq (Maybe m) where
   Just x == Just y     = x == y
   Nothing == Nothing   = True
   _ == _               = False

--this is good version with class constraint
instance (Eq m) => Eq (Maybe m) where
   ...


-- # ex 3 (yes-no typeclass):

--type coercion like js

class YesNo a where
   yesno :: a -> Bool

instance YesNo Int where
   yesno 0 = False
   yesno 1 = True

instance YesNo [a] where
   yesno [] = False
   yesno _ = True

instance YesNo Bool where
   yesno = id

instance YesNo (Maybe a) where
   yesno (Just _) = True
   yesno Nothing = False

yesnoIf :: (YesNo y) => y -> a -> a -> a
yesnoIf yesnoVal yesResult noResult = if yesno yesnoVal then yesResult else noResult




---- ### Functor typeclass

class Functor f where
   fmap :: (a -> b) -> f a -> f b

--f is a type constructor that takes type argument

instance Functor [] where
   fmap = map

--not "instance Functor [a] where" because f
--has to be type constructor that takes 1  type

instance Functor Maybe where
   fmap f (Just x) = Just (f x)
   fmap f Nothing = Nothing

instance Functor Tree where
   fmap f EmptyTree = EmptyTree
   fmap f (Node x leftsub rightsub) = Node (f x) (fmap f leftsub) (fmap f rightsub)

instance Functor (Either a) where
   fmap f (Right x) = Right (f x)
   fmap f (Left x) = Left x

--functor laws:
--identity: fmap (\a -> a) should return same thing


---- ### kinds

-- values can only be concrete types
-- * means concrete type

-- # exs:

--:k Int
--Int :: *

--:k Maybe
--Maybe :: * -> *

--:k Maybe Int
--Maybe Int :: *

--:k Either
--Either :: * -> * -> *

--:k Either String
--Either String :: * -> *


--:k Functor
--Functor :: (* -> *) -> GHC.Prim.Constraint

--:k Eq
--Eq :: * -> GHC.Prim.Constraint


-- ## type-foo tofu

class Tofu t where
   tofu :: j a -> t a j

--:k Tofu
--Tofu :: (* -> (* -> *) -> *) -> GHC.Prim.Constraint

--:t tofu
--tofu :: Tofu t => j a -> t a j

--because "j a" is parameter, then it's :k is *
--assume * for a
--j has to have :k of "* -> *"
--t produces concrete type and takes 2 types
   -- :k t = * -> (* -> *) -> *

-- # Frank

data Frank a b = Frank {frankField :: b a} deriving (Show)
--:k Frank :: * -> (* -> *) -> *

--ex1:
--Frank (Just 3)
   --Frank { frankField = Just 3}

--:t Frank (Just 3)
   --Frank (Just 3) :: Num a => Frank a Maybe

--:k Frank (Just 3)
   -- Illegal type: 3


--ex2:
--:t Frank "Fds"
   -- Frank "Fds" :: Frank Char []


instance Tofu Frank where
   tofu x = Frank x

let x = tofu "fds"
--:t x
   -- x :: Tofu t -> t Char []



-- ## type-foo Bary

data Barry t k p = Barry { yabba :: p, dabba :: t k }
--:k Barry --     t       k    p
   --Barry :: (* -> *) -> * -> * -> *

instance Functor (Barry a b) where
   fmap f (barry {yabba = x, dabba = y}) = Barry {yabba=f x, dabba=y}





