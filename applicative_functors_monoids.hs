
---- #### Functors

--- ### Functor typeclass

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

x = fmap (+3) $ Just 5 --x = Just 7
x = fmap (+3) $ Nothing --x = Nothing


--functor laws:
--identity: fmap (\a -> a) should return same thing



-- ### IO functor

instance Factor IO where
   fmap f action = do
      result <- action
      return (f result)

--ex1: no functor
main_ex1 = do
   line <- getLine
   let line' = reverse line
   putStrLn $ "Line backwards:" ++ line'

--ex2: with functor
main_ex2 = do
   line <- fmap reverse getLine
   putStrLn $ "Line backwards:" ++ line

--ex3 (function composition):
main = do
   line <- fmap (intersperse '-' . reverse . map toUpper) getLine
   putStrLn line

--in: hello there
--out: E-R-E-H-T- -O-L-L-E-H



-- ### (->) r

instance Functor ((->) r) where
   fmap f g (\x -> f (g x))

--this syntax doesn't work
--instance Functor (r ->) where
--   fmap f g (\x -> f (g x))


instance Functor ((->) r) where
   fmap = (.)

--fmaps over result
--fmap (*100) (+2) $ 4
--returns (4+2)*100 = 600

--(*100) `fmap` (+2)
--same as
--(*100) . (+2)



--- ### Functor laws

-- instance of Functor, but not really being a functor
data CMaybe a = CNothing | CJust Int a deriving (Show)

instance Functor CMaybe where
   fmap f CNothing = CNothing
   fmap f (CJust counter x) = CJust (counter+1) (f x)


--fmap (++"ha") (CJust 0 "ho")
--returns: CJust 1 "hoha"

--functor laws broken:

--fmap id $ CJust 0 "haha"
--returns: CJust 1 "ha"

--id (CJust 0 "haha")
--returns: CJust 0 "haha"



---- #### Aplicatives

--- ### Applicative Typeclass

-- # ex1: definition

--import Control.Applicative
class (Functor f) => Applicative f where
   pure :: a -> f a
   (<*>) :: f (a -> b) -> f a -> f b

--fmap as infix operator
(<$>) :: (Functor f) => (a -> b) -> f a -> f b
f <$> x = fmap f x


-- # examples

-- ex2: Maybe as applicative
instance Applicative Maybe where
   pure = Just
   Nothing <*> _ = Nothing
   (Just f) <*> something = fmap f something

--not "instance Applicative Maybe a where" because
--f (applicative functor) should take one concrete type as parameter


--ex3: List as Applicative
instance Applicative [] where
   pure x = [x]
   fs <*> xs = [f x | f <- fs, x <- xs]



-- # IO applicative

--ex4 (IO as applicative):
instance Applicative IO where
   pure = return
   a <*> b = do
      f <- a
      x <- b
      return (f x)


--ex 5 normal vs applicative IO

--normal:
myAction :: IO String
myAction = do
   a <- getLine
   b <- getLine
   return $ a ++ b

--applicative:
myAction :: IO String
myAction = (++) <$> getLine <*> getLine



-- ## (->) r as applicative

instance Applicative ((->) r) where
   -- pure :: a -> (r -> a)
   pure x = (\_ -> x)
   f <*> g = \x -> f x (g x)

-- # ex 6:

--(pure 3) "blah"
--or
--pure 3 "blah"

--both return 3


-- # ex 7:

x = (+) <$> (+3) <*> (*100) $ 5
--result: 5+3 + 5*100


-- #ex 7:
--(\x y z -> [x,y,z]) <$> (+3) <*> (*2) <*> (/2) $ 5


-- ## ZipList as applicative

instance Applicative ZipList where
   pure x = ZipList (repeat x)
   ZipList fs <*> ZipList xs = ZipList (zipWith (\f x -> f x) fs xs)


-- ## sequenceA

--v1:
sequenceA :: (Applicative f) => [f a] -> f [a]
sequenceA [] = pure []
sequenceA (x:xs) = (:) <$> x <*> sequenceA xs

--v2:
sequenceA :: (Applicative f) => [f a] -> f [a]
sequenceA = foldr (liftA2 (:)) (pure [])




