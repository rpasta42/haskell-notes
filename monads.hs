
-- ## fmap

--fmap :: (Functor f) => (a -> b) -> f a -> f b

--ex1:
x1 = fmap (++"!") (Just "wisdom")
--returns: Just "wisdom!"



-- ## Applicative

--(<*>) :: (Applicative f) => f (a -> b) -> f a -> f b

--ex2:
x2 = Just (+3) <*> Just 3
--returns: Just 6


--ex3:
x3 = Nothing <*> Just "test"
--returns: Nothing


--ex4:
x4 = Just (+) <*> Nothing
--nothing


--ex5:
x5 = max <$> Just 3 <*> Just 6
--return Just 6


--ex6:
x6 = max <$> Just 3 <*> Nothing
--returns Nothing



-- ## Monad

-- # monad examples

--(>>=) :: (Monad m) => m a -> (a -> m b) -> m b

returnMaybe :: (Num a, Ord a) => a -> Maybe a
returnMaybe x = if x > 10 then Just x else Nothing

f_monad :: (Monad t) => t a -> (a -> t a) -> t a
f_monad x func = x >>= func

--bad:
--f_monad :: Maybe a -> (a -> Maybe a) -> Maybe a
--f_monad x func = func >>= x

--ex7/ex8:
x7 = f_monad (Just 54) returnMaybe -- Just 54
x8 = f_monad (Just 5) returnMaybe -- Nothing


--ex9/ex10:
x9 = Just 5 >> Just 6 --Just 6
x10 = Nothing >> Just 6 --Nothing


-- # ex 11-14 (making monads Maybe):

data Maybe1 a = Just1 a | Nothing1 deriving (Show)

instance Functor Maybe1 where
   fmap _ Nothing1 = Nothing1
   fmap f (Just1 x) = Just1 $ f x

instance Applicative Maybe1 where
   pure = Just1
   Nothing1 <*> _ = Nothing1
   (Just1 f) <*> something = fmap f something

instance Monad Maybe1 where
   return x = Just1 x
   Nothing1 >>= f = Nothing1
   Just1 x >>= f  = f x
   fail _ = Nothing1

-- ex 11 (Monad usage):
x11 = Just1 3 >>= (\x -> Just1 $ x+5) --Just1 8

--ex 12 (Monad):
x12 = Just1 9 >>= \x -> return (x*10) --Just1 90

--ex 13 (Applicative usage):
x13 = (+) <$> Just1 3 <*> Just1 5 -- Just1 8

--ex 14 (Applicative):
x14 = Just1 (3+) <*> Just1 5 --Just1 8



-- # ex 15 (tightrope example)

type Birds = Int
type Pole = (Birds, Birds)

--ex15.1:
landLeft1 :: Birds -> Pole -> Pole
landLeft1 x (l,r) = (l+x, r)

landRight1 :: Birds -> Pole -> Pole
landRight1 x (l,r) = (l, r+x)

x -: f = f x
-- 3 -: (+5) instead of (+5) 3

pole1 = (0,0) -: landLeft1 1 -: landRight1 1 -: landLeft1 2


--ex15.2:
landLeft :: Birds -> Pole -> Maybe Pole
landLeft n (left,right)
   | abs ((left+n) - right) < 4 = Just (left + n, right)
   | otherwise = Nothing

landRight :: Birds -> Pole -> Maybe Pole
landRight n (left,right)
   | abs ((right+n) - left) < 4 = Just (left, right + n)
   | otherwise = Nothing


pole2 = landRight 1 (0,0) >>= landLeft 2
--Just (2,1)

pole3 = return (0,0) >>= landRight 2 >>= landLeft 2 >>= landRight 2
--Just (2,4)

pole4 = pole3 >>= landLeft 2
--Just (4,4)

pole5 = pole3 >>= landRight 2
--Nothing



--ex16 (slip and fall):

banana :: Pole -> Maybe Pole
banana _ = Nothing

x16 = return (0,0) >>= landLeft 1 >>= banana >>= landRight 1
--result: Nothing


--ex17 (>>)

--instead of making a function that ignore their input and just return
--a predetermined monadic value, we can use >>

x17_1 = Nothing >> Just 3 --Nothing
x17_2 = Just 3 >> Just 4 --Just 4
x17_3 = Just 3 >> Nothing --Nothing

--ex18 (replacing banana)

x18 = return (0,0) >>= landLeft 1 >> Nothing >>= landRight 1


-- # do notation

--ex19 (do notation)

--instead of doing this
x19_1 = (Nothing :: Maybe String)
        >>= (\x -> Just "!"
                   >>=
                  (\y -> Just $ show x ++ y))

--do notation
x19_2 = do
   x <- Just 3
   y <- Just "!"
   Just (show x ++ y)


