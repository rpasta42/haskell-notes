
- lazy

- statically typed
   - type inference

- ghci
   - needs lets for functions and variables
   - to load a file
      - write file.hs
      - load with :l file
      - to reload, either :l file again, or just :r

- misc
   - ' is a valid character in names

- list
   - comparison is lexicographical
      - first compares heads
         - if first greater, returns True
         - if equal, compares second element

- tuples
   - can contain different type elements
   - type of a tuple depends on length

- types
   - Int, Integer Char, Bool, Float, Double
      - Float short precision
      - Double long precision
      - Integer very long integer
      - Int short integer but faster
      - () is a type which only has one value: ()
   - ex 1:
      ghci> :t 'a'
      'a' :: Char
   - ex 2:
      ghci> :t "Hello"
      "Hello" :: [Char]
   - ex 3:
      ghci> :t (True, 'a')
      (True, 'a') :: (Bool, Char)

   - lazy evaluation
      - thunk = promise to compute later when asking to
        produce result


- typeclasses
   - Eq, Ord, Show, Read, Enum, Bounded, Num,
     Integral
   - ex 1:
      ghci> :t (==)
      (==) :: (Eq a) => a -> a -> Bool

   - Ord
      - `compare` returns Ordering type
         - Ordering: LT, GT, EQ
      - covers >, <, >=, <=
      - to be member of Ordering, a type needs to be part of Eq typeclass
   - Show
      - can be presented as strings
      - uses show function
   - Read
      - opposite of show. takes string and converts to type
      - ex 1: read "5" + 3.8
      - ex 2: read "[1, 2, 3]" ++ [3]
      - ex 3: read "5" :: Int
      - ex 4: (read "5" :: Float) * 4
   - Enum
      - orderable
      - can be used in List ranges
   - Bounded
      - have upper and lower limit
      - ex 1: minBound :: Int
      - ex 2: maxBound :: Char
      - ex 3: maxBound :: (Bool, Int, Char)
        - out: (True, 2147483648, '\1114111')
   - Numeric typeclasses
      - Num
         - numeric
         - must already be in Show and Eq typeclasses
         - ex: ghci> :t (*)
            (*) :: Num a => a -> a -> a
         - polymorphic constant
            - can act as any type
            - ex: ghci> :t 2
               2 ::(Num t) => t
      - Integral
         - includes only whole numbers
         - Int and Integer are Integral typeclass
      - Floating
         - includes only floating point numbers
         - Float and Double are Floating typeclass
      - fromIntegral
         - takes Integral number and turns it into
           generic Num
         - fromIntegral :: (Num b, Integral a) => a -> b
         - ex: ghci> fromIntegral (length [1, 2, 3]) + 3.2

   - folds (foldl, foldr, foldl1, foldr1)
      - foldr1 foldl1 don't take starting argument
      - foldl' and foldl1' are stricter than foldr

   - scan (scanl, scanr, scanl1, scanr1)
      - scanl scanr like folds, but reports intermediary results in a list



- Modules
   - loosely coupled = components don't rely on each
     other too much

   - importing
      - put module in global namespace in ghci
         - `:m + Data.List Data.Map`
         - if loaded a script that already imports
           module, don't need to do `:m +`
      - only import certain stuff from module
         - `import Data.List (nub, sort)`
      - import everything, except few functions
         - `import Data.List hiding (nub)`
      - qualified import, so need to use module name
         - `import qualified Data.Map`
      - qualified with custom name
         - `import qualified Data.Map as M`
         - usage ex: `M.filter`

   - Haskell standard library is split into Modules
      - Prelude modules is loaded by default
      - Concurrency module
      - Complex Numbers module

   - STD module:
      - Data.List
         - List Manipulation module
            - `import Data.List`
            - `nub :: Eq a => [a] -> [a]`
            - returns unique elements from list, and
              takes out duplicates
         - intersperse
            - `intersperse :: a -> [a] -> [a]`
            - `intersperse '.' "MONKEY"`
               - returns "M.O.N.K.E.Y"
         - intercalate
            - `intercalate :: [a] -> [[a]] -> [a]`
            - `intercalate " " ["hey", "there", "guys"]`
               - returns "hey there guys"
            - `intercalate [0, 0] [[1, 2], [3, 4]]`
               - returns `[1, 2, 0, 0, 3, 4]`
         - transpose
            - in 2D matrix, turns columns into rows
            - `transpose [[1, 2], [4, 5]]`
               - returns `[[1, 4], [2, 5]]`
         - concat
            - flattens list of lists into 1 list
               - `concat ["foo","bar","car"]`
                  - returns "foobarcar"
         - concatMap
            - first maps a function to list, and
              then concatenates with concat
            - `concatMap (replicate 2) [1..3]`
               - returns [1,1,2,2,3,3]
         - and
            - takes list and returns only if all
              elements are True
            - `and $ map (>4) [5, 6, 7, 8]`
               - returns True
         - or
            - returns True if any element is True
            - `or $ map (>4) [1,2,5]`
               - returns True
            - `or $ map (>4 [1, 2, 3]`
               - returns False
         - any and all
            - take predicate and then check if
              any/all elements satisfy the predicate
            - `any (==4) [2, 3, 4]`
               - returns True
            - `all (>4) [5, 6, 7]`
               - returns True
         - iterate
            - takes a function and starting value,
              applies the function to starting
              value, then applies function to
              result
            - `:t iterate`
               - `iterate :: (a -> a) -> a -> [a]`
            - `take 5 $ iterate (*2) 1`
               - returns [1, 2, 4, 8, 16]
         - splitAt
            - takes index number and list, split at index,
              returns tuple
            - `:t splitAt`
               - `splitAt :: Int -> [a] -> ([a], [a])`
            - `splitAt 3 "heyman"`
               - returns `("hey", "man")`
         - takeWhile
            - takes while predicate is true
            - `:t takeWhile`
               - `takeWhile :: (a->Bool) -> [a] -> [a]`
            - `takeWhile (>3) [6,5,3,9]`
               - returns `[6, 5]`
            - `takeWhile (/=' ') "This is a test"
               - returns "This"
         - dropWhile
            - same as takeWhile, but drop
            - dropWhile (/=' ') "This is a sentence"
               - returns " is a sentence"
         - span
            - similar to takeWhile, returns tuples of lists.
            - First part is takeWhile, second is stuff
              that would've gotten dropped
            - `:t span`
               - `span :: (a -> Bool) -> [a] -> ([a], [a])`
            - `span (/=' ') "Hello world test"`
               - returns `("Hello", " world test")`
         - sort
            - sort list, with elements which are
              part of Ord typeclass
            - `:t sort`
               - `sort :: Ord a => [a] -> [a]`
            - `sort "This will be sorted soon"`
               - returns "     Tbdeehillnooorssstw"
         - group
            - takes list and groups adjacent elements
              if they are equal
            - `:t group`
               - `group :: Eq a => [a] -> [[a]]`
            - `group [1,1,1,2,2,3,3,2,2,5,6,7]`
               - returns `[[1,1,1], [2,2], [3,3], [2, 2,], [5], [6], [7]]`
         - inits & tails
            - recursive apply init and tail
            - `inits "w00t"`
               - returns `["", "w", "w00", "w00t"]`
            - `tails "w00t"`
               - returns `["w00t", "00t", "0t", "t", ""]`
            - `let w = "w00t" in zip (inits w) (tails W)`
               - returns `[("", "w00t"), ("w", "00t"), ("w0", "0t"), ("w00", "t"), ("w00t", "")]`
         - isInfixOf
            - takes 2 lists and checks if first is
              sublist of second
            -  `:t isInfixOf`
               - `isInfixOf :: Eq a => [a] -> [a] -> Bool`
            - `isInfixOf "cat" "im a cat burglar"`
               - returns : True
         - isPrefixOf & isSuffixOf
            - searches for sublist at beginning
              and end of list
            - `isPrefixOf "key" "key there"`
               - returns: True
            - `isSuffixOf "there!" "Hey there!"`
               - returns: True
         - elem/notElem
            - checks if element is/isn't inside a list
            - `elem 'a' ['A'..'Z']`
               - returns: False
         - partition
            - takes a predicate and a list, returns
              a pair of 2 lists based on whether an
              element satisfies predicate
            - `:t partition`
               - `partition :: (a -> Bool) -> [a] -> ([a], [a])`
            - partition ```haskell (`elem` ['A'..'Z'] "Hello World")```
               - returns: `("HW", "ello orld")`


- TODO:
   - implement foldl1 foldr1, scans, etc


