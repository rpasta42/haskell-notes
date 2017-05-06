import Data.Monoid
import qualified Data.Foldable as F

---- #### Monoids

-- # ex1 ([]):

--instance Monoid [a] where
--   mempty = []
--   mappend = (++)


-- # ex2 (MyList):
data MyList a = Cons a (MyList a) | LEmpty
                  deriving (Show)

myListSum :: MyList a -> MyList a -> MyList a
myListSum LEmpty lst2 = lst2
--myListSum (Cons carLst1 cdrLst1) lst2 = myListSum cdrLst1 (Cons carLst1 lst2)
myListSum (Cons carLst1 cdrLst1) lst2 = Cons carLst1 (myListSum cdrLst1 lst2)

instance Monoid (MyList a) where
   mempty = LEmpty
   mappend = myListSum

--mconcat [Cons 1 LEmpty, LEmpty, Cons 1 (Cons 3 LEmpty)]
--Cons 1 (Cons 1 (Cons 3 LEmpty))


-- # ex3 (Num with with newtype):
newtype ProductK a = ProductK { getProductK :: a }
   deriving (Eq, Ord, Read, Show, Bounded)

instance Num a => Monoid (ProductK a) where
   mempty = ProductK 1
   ProductK x `mappend` ProductK y = ProductK (x*y)


-- # ex4 (Any):

newtype Any1 = Any1 { getAny1 :: Bool }
   deriving (Eq, Ord, Read, Show, Bounded)

instance Monoid Any1 where
   mempty = Any1 False
   mappend (Any1 x) (Any1 y) = Any1 (x || y)


-- # ex5 (All):
newtype All1 = All1 { getAll1 :: Bool }
   deriving (Eq, Ord, Read, Show, Bounded)

instance Monoid All1 where
   mempty = All1 True
   All1 x `mappend` All1 y = All1 (x && y)


-- ## ex6 (Ordering):

data Ordering1 = LT1 | EQ1 | GT1
   deriving (Show)

--left parameter is always kept unless it's EQ
instance Monoid Ordering1 where
   mempty = EQ1
   LT1 `mappend` _ = LT1
   EQ1 `mappend` y = y
   GT1 `mappend` _ = GT1

--compare length, returns ordering
--if strings of same length, compare strings alphabetically

-- # v1:
lengthCompare1 :: String -> String -> Ordering
lengthCompare1 s1 s2 =
   let ord1 = length s1 `compare` length s2
       ord2 = s1 `compare` s2
   in if ord1 == EQ
      then ord2
      else ord1


-- # v2 (with Monoids):
lengthCompare2 :: String -> String -> Ordering
lengthCompare2 s1 s2 = (length s1 `compare` length s2) `mappend`
                       (s1 `compare` s2)


-- # v3 (if we want to compare number of vowels as second parameter):
lengthCompare3 s1 s2 = (length s1 `compare` length s2) `mappend`
                       (vowels s1 `compare` vowels s2) `mappend`
                       (s1 `compare` s2)
   where vowels = length . filter (`elem` "aeiou")



-- ## ex7 (Maybe):

-- # method 1 (real default)

--only if type parameter a is a monoid
--use mappend on values that are wrapped in Just
--use Nothing as identity
--if one of the 2 values we're mappending is Nothing, we keep the other value

data Maybe1 a = Just1 a | Nothing1

instance Monoid a => Monoid (Maybe1 a) where
   mempty = Nothing1
   Nothing1 `mappend` m = m
   m `mappend` Nothing1 = m
   Just1 m1 `mappend` Just1 m2 = Just1 (m1 `mappend` m2)

--ex:
--Just [5] `mappend` Just [3]
--returns Just [5, 3]


-- # method 2 (First)

--if type parameter a isn't a Monoid
--we can discard the second value

newtype First1 a = First1 { getFirst1 :: Maybe a }
   deriving (Eq, Ord, Read, Show)

instance Monoid (First1 a) where
   mempty = First1 Nothing
   First1 (Just x) `mappend` _ = First1 (Just x)
   First1 Nothing `mappend` x = x

--ex:
--(First $ Just 5) `mappend` (First $ Just 65) `mappend` (First $ Nothing)
--returns: First {getFirst = Just 5}




---- #### Foldable

-- # Maybe

--F.foldl (+) 2 (Just 9)
--returns: 11

--F.foldr (||) False (Just True)
--returns: True


-- # Trees
data Tree a = Empty | Node a (Tree a) (Tree a)
               deriving (Show, Read, Eq)

instance F.Foldable Tree where
   foldMap f Empty = mempty

   foldMap f (Node x l r) = F.foldMap f l `mappend`
                            f x           `mappend`
                            F.foldMap f r

testTree = Node 5
           (Node 3
               (Node 1 Empty Empty)
               (Node 6 Empty Empty)
            )
            (Node 9
               (Node 8 Empty Empty)
               (Node 10 Empty Empty)
            )

testTreeFoldlSum = F.foldl (+) 0 testTree --42
testTreeFoldrSum = F.foldr (+) 0 testTree --42

testTreeFoldrProduct = F.foldr (*) 1 testTree --64,800

testTreeFoldrList = F.foldr (:) [] testTree
--result: [1,3,6,5,8,9,10]

testTreeMapList = F.foldMap (\x -> [x]) testTree
--result: [1,3,6,5,8,9,10]

testTreeAny = getAny $ F.foldMap (\x -> Any $ x == 3) testTree



