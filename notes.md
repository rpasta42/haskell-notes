
- misc
   - lazy
   - statically typed
      - type inference
   - cabal
      - `apt install cabal-install`
      - `cabal update`
         - update package list
      - ex
         - `cabal install random`

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
      - empty tuple is called unit
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


   - folds (foldl, foldr, foldl1, foldr1)
      - foldr1 foldl1 don't take starting argument
      - foldl' and foldl1' are stricter than foldr

   - scan (scanl, scanr, scanl1, scanr1)
      - scanl scanr like folds, but reports intermediary results in a list


- typeclasses
   - Eq, Ord, Show, Read, Enum, Bounded, Num,
     Integral
   - ex 1:
      - `ghci> :t (==)`
      - `(==) :: (Eq a) => a -> a -> Bool`

   - Ord
      - `compare` returns Ordering type
         - Ordering: `LT`, `GT`, `EQ`
      - covers >, <, >=, <=
      - to be member of Ordering, a type needs to be part of Eq typeclass
   - Show
      - can be presented as strings
      - uses show function
   - Read
      - opposite of show. takes string and converts to type
      - ex 1: `read "5" + 3.8`
      - ex 2: `read "[1, 2, 3]" ++ [3]`
      - ex 3: `read "5" :: Int`
      - ex 4: `(read "5" :: Float) * 4`
   - Enum
      - orderable
      - can be used in List ranges
   - Bounded
      - have upper and lower limit
      - ex 1: `minBound :: Int`
      - ex 2: `maxBound :: Char`
      - ex 3: `maxBound :: (Bool, Int, Char)`
        - out: `(True, 2147483648, '\1114111')`
   - Numeric typeclasses
      - Num
         - numeric
         - must already be in Show and Eq typeclasses
         - ex: `ghci> :t (*)`
            - `(*) :: Num a => a -> a -> a`
         - polymorphic constant
            - can act as any type
            - ex: `ghci> :t 2`
               - `2 ::(Num t) => t`
      - Integral
         - includes only whole numbers
         - Int and Integer are Integral typeclass
      - Floating
         - includes only floating point numbers
         - Float and Double are Floating typeclass
      - fromIntegral
         - takes Integral number and turns it into generic Num
         - `fromIntegral :: (Num b, Integral a) => a -> b`
         - ex: `ghci> fromIntegral (length [1, 2, 3]) + 3.2`
   - Functor
      - fmap
         - `fmap :: Functor f => (a -> b) -> f a -> f b`
   - Applicative
      - `Control.Applicative` module
      - functions
         - `pure : a -> f a`
         - `(<*>) :: f (a -> b) -> f a -> f b`
         - `(<$>)`
            - fmap as infix operator
            - `(<$>) :: (Functor f) => (a -> b) -> f a -> f b`
            - `f <$> x = fmap f x`

   - `Data.Monoid` type class
      - ```haskell
         class Monoid m where
            mempty :: m
            mappend :: m -> m -> m
            mconcat :: [m] -> m
            mconcat = foldr mappend mempty
         ```



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


   - non STD modules

      - System.Random
         - installation
            - `cabal install random`
         - typeclasses
            - RandomGen
         - types
            - StdGen
            - Random

         - functions
            - mkStdGen
               - `mkStdGen :: Int -> StdGen`
            - random
               - `random :: (RandomGen g, Random a) => g (a, g)`
               - `random $ mkStdGen 100`
                  - `(-3633736515773289454,693699796 2103410263)`
                     - first value is random number, second is new RandomGen
            - randoms
               - infinite list of randoms
            - randomR
               - `randomR :: (RandomGen g, Random a) -> (a, a) -> g -> (a, g)`
               - random in range
            - randomRs
               - stream of randoms in a range
               - `randomRs :: (RandomGen g, Random a) => (a, a) -> g -> [a]`
               - ex: `take 10 $ randomRs ('a','z') (mkStdGen 3)`
            - getStdGen
               - `getStdGen :: IO StdGen`
               - non-pure so random numbers won't be the same each time
               - calling twice, will get the same generator twice
            - newStdGen
               - `newStdGen :: IO StdGen`
               - new random number generator
               - if call getStdGen somewhere, it'll be the new generator



   - STD module:
      - prelude
         - `id`
            - takes parameter, returns same thing

      - `Data.ByteString`
         - notes
            - `Data.ByteString`
               - default one is strict
               - no promises, no infinite bytestrings
               - fills up memory faster
            - `Data.ByteString.Lazy`
               - lazy version
               - has thunks (promise)
               - stored in chunks
                  - in a list, each element is a thunk
                  - each chunk is 64K
                  - fits in CPU L2 cache
            - each element is 1 byte in size
         - functions
            - imports
               - `import qualified Data.ByteString.Lazy as B`
               - `import qualified Data.ByteString as S`
               - `import GHC.Word`
                  - like `Int`, but 0-255 range
                  - in `Num typeclass`
            - pack
               - `B.pack`
                  - `pack :: [Word8] -> Data.ByteString.Lazy.ByteString`
               - `S.pack`
                  - `pack :: S.pack :: [Word8] -> Data.ByteString.ByteString`
               - ex: `B.pack [88..100]`
            - unpack
               - takes a bytestring and turns it into a list of bytes
            - fromChunks
               - takes a list of strict bytestring and converts them into lazy bytestrings
               - good if have a lot of small strict bytestrings and want to process
                 them efficiently without joing them into one big strict bytestring
               -ex: `B.fromChunks [S.pack [40,41,42], S.pack [43,44,45], S.pack [46,47,48]]`
            - cons
               - same as `:` for lists
               - B.cons: makes a new chunk even if the first chunk in bytestring isn't full
            - `cons'`
               - strict, good if inserting a lot of bytes
            - empty
               - makes empty bytestring
            - head/tail/init/null/length/map/reverse/foldl/foldr/concat/takeWhile/filter
               - same functions as lists
            - functions from System.IO except ByteString instead of string
               - readFile
                  - `:t readFile :: FilePath -> IO ByteString`



      - Data.Char
         - Other functions
            - generalCategory
               - `:t generalCategory`
                  - `generalCategory :: Char -> GeneralCategory`
               - GeneralCategory
                  - `Space`/`UppercaseLetter`/`LowercaseLetter`/
                    `OtherPunctuation`/`DecimalNumber`/`Control`/`MathSymbol`
            - toUpper = converts Char to upper-case
            - toLower = converts to lower-case
            - toTitle = same as uppercase
            - digitToInt = converts char to Int
               - needs to be `['0'..'9'] || ['a'..'f'] || ['A'..'F']`
            - intToDigit = opposite of digitToInt
               - `intToDigit 15 => 'f'`
            - ord = convert to ASCII number
               - `ord 'a' => 97`
            - chr = convert number to ASCII char
               - `chr 97 => 'a'`
         - predicates
            - `Char -> Bool`
            - Predicate Functions
               - `isControl` = checks if control character
               - `isSpace` = is space/tab/newline
               - `isLower` = is lowercase
                  - returns False on space
               - `isUpper` = is uppercase
               - `isAlpha` = is letter
               - `isAlphaNum` = letter or number
               - `isPrint` = is printable
                  - control characters aren't printable
               - `isDigit` = is digit
               - `isOctDigit` = is octal digit
               - `isHexDigit` = is hex digit
               - `isLetter` = is letter
               - `isMark` = unicode Mark character
                  - check for letters with accent
               - `isNumber` = is numeric
               - `isPunctuation` = is punctuation
               - `isSymbol` = math/currency symbol
               - `isSeparator` = Unicode spaces and separators
               - `isAscii` = first 128 characters of Unicode
               - `isLatin1` = first 256 chars of Unicode
               - `isAsciiUpper` = ASCII uppercase
               - `isAsciiLower` = ASCII lowercase
            - ex: `all isAlphaNum "bobby283"`
               - returns `True`
            - ex: simulate words
               - `filter (not . any isSpace) . groupBy ((==) `on` isSpace) $ "hey guys its me"`

      - Data.Map
         - association list (aka dictionary) for key-value pairs
         - Functions
            - `Map.fromList`
               - `:t Map.fromList`
                  - `Map.fromList :: Ord k => [(k, a)] -> Map.Map k a`
               - `import qualified Data.Map as Map`
               - `Map.fromList [("key", "val"), ("key", "val")]`
                  - converts list to map
            - `Map.empty`
               - `ghci>Map.empty`
                  - returns `fromList []`
            - `Map.insert`
               - `Map.insert 3 100 Map.empty`
                  - returns `fromList [(3, 100)]`
            - `Map.null`
               - returns if Map is empty
               - `Map.null Map.empty`
                  - returns `True`
            - `Map.size`
               - number of key-item pairs
            - `Map.singleton`
               - returns map with exactly one mapping
               - `Map.insert 5 9 $ Map.singleton 3 9`
                  - returns `fromList [(3,9), (5, 9)]`
            - `Map.member`
               - predicate that takes key and map and
                 returns whether the key is in the map
               - `Map.member 3 $ Map.fromList [(2, 5), (4, 5)]`
                  - returns `False`
            - `Map.lookup`
               - takes key and returns Maybe (Just val
               - if key is in Map, otherwise Nothing)
            - `Map.map`
               - like list map
            - `Map.fiter`
               - like list filter
            - `Map.toList`
               - convert map to list (opposite of fromList)
            - `Map.keys`
               - returns all keys
               - same as `map fst . Map.toList`
            - `Map.elems`
               - returns all elements of Map
               - same as `map snd . Map.toList`
            - `Map.fromListWith`
               - takes function to combine duplicates
               - `:t Map.fromListWith`
                  - `:: Ord k => (a -> a - a) -> [(k, a)] -> Map.Map k a`
               - phonebook has multiple numbers for same person,
                 so we add them to one key using comma
                  - `phoneBookToMap xs = Map.fromListWith (\ number1 number2 -> number1 ++ ", " ++ number2) xs`
                  - can also make every value in phonebook a singleton list
                     - `phoneBookToMap :: (Ord k) => [(k, a)] -> Map.Map k [a]`
                     - `phoneBookToMap xs = Map.fromListWith (++) $ map (\(k, v) -> (k, [v])) xs`
               - map with list of number pairs, duplicate selects biggest number
                  - `Map.fromListWith max [(2, 3), (2, 5), (3, 10)]`
                     - returns `fromList [(2, 5), (3, 10)]`
               - map with list of number pairs, duplicate adds both numbers
                  - `Map.fromListWith (+) [(2, 3), (2, 5)]`
            - `Map.insertWith`
               - if key is already in map, determines what to add instead
               - function argument: pair of values with equivelent keys
               - `Map.insertWith (+) 3 100 $ Map.fromList [(3, 4), (5, 6)]`
                  - returns `fromList [(3, 104), (5, 6)]`


      - Data.Set
         - all elements are unique
         - can map and filter over sets
         - converting list to from set is a lot faster than using nub
         - ordered
            - stored in trees
            - checking membership, inserting, deleting
              is much faster than List
         - clashes Prelude data so need to import like this:
            - `import qualified Data.Set as Set`
         - Functions
            - `Set.fromList`
               - `:t Set.fromList`
                  - `Set.fromList :: Ord a => [a] -> Set.Set a`
               - `Set.fromList "Hello world"`
                  - returns `fromList " Hdelorw"`
            - `Set.toList`
            - `Set.intersection`
               - takes 2 sets and returns elements in common
            - `Set.difference`
               - takes 2 sets
               - returns elements in first set, which are not in second
            - `Set.union`
               - union of 2 sets (all together)
            - `Set.null`
               - is empty set
            - `Set.empty`
               - returns empty set
            - `Set.size`
               - returns length of a set
            - `Set.singleton`
               - returns set with 1 item
               - `Set.singleton 9`
            - `Set.insert`
               - insert into set and return a new one
               - `Set.insert 4 $ Set.fromList [5..10]`
            - `Set.delete`
               - delete item from set
               - `Set.Delete 4 $ Set.fromList [3..5]`
            - `Set.isSubsetOf`
            - `Set.isProperSubsetOf`
            - `Set.map`
            - `Set.filter`



      - Data.Function
         - on
            - `:t on`
               - `on :: (b -> b -> c) -> (a -> b) -> a -> a -> c`
            - definition
               - ``` f `on` g = \x y -> f (g x) (g y)```
            - `on (==) (> 0)`
               - same as `\x y -> (x > 0) == (y > 0)`

      - Data.List
         - List Manipulation module
            - `import Data.List`
            - `nub :: Eq a => [a] -> [a]`
            - returns unique elements from list, and
              takes out duplicates
         - generic versions
            - length, take, drop, splitAt, !!, replicate
              return Int instead of Integral or Num
            - genericLength, genericTake, genericDrop,
              genericSplitAt, genericIndex, genericReplicate
         - by functions
            - nub, delete, union, intersect, group
               - use (==) to test for equality
            - nubBy, deleteBy, unionBy, intersectBy, groupBy
               - first argument is equality function
                  - `a -> b -> Bool`
               - often used with Data.Functions's on
               - ``` groupBy ((==) `on` (> 0)) values ```
            - sortBy, insertBy, maximumBy, minimumBy
               - take a function that returns Ordering
                  - `a -> a -> Ordering`
               - `:t sortBy`
                  - `sortBy :: (a -> a -> Ordering) -> [a] -> [a]`
               - Ordering can be LT, EQ, GT
               - `sort` is same as `sortBy compare`
            - compare lists based on length instead of lexicographically
               - ```sortBy (compare `on` length) [[], [1, 2, 5], [1, 2]]```

         - Functions
            - lookup
               - `lookup :: Eq a => [(a, b)] -> Maybe b`
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
            - find
               - takes a list and a predicate, returns
                 first element that satisfies predicate
               - `:t find`
                  - `find :: (a -> Bool) -> [a] -> Maybe a`
               - `find (>3) [1, 3, 4, 8]`
                  - returns `Just 4`
               - `find (>9) [1, 2, 4, 8`
                  - returns `Nothing`
            - elemIndex
               - similar to elem, but returns `Maybe Int`
               - `:t elemIndex`
                  - `elemIndex :: (Eq a) => a -> [a] -> Maybe Int`
               - `elemIndex 4 [1, 2, 3, 4, 5]`
                  - returns `Just 3`
            - elemIndices
               - same as elemIndices, but returns all
                 of them instead of just first
               - failure is empty list
            - findIndex/findIndices
               - same as find, but with indices
               - `:t findIndex`
                  - `findIndex :: (a -> Bool) -> [a] -> Maybe Int`
               - `:t findIndices`
                  - `findIndices :: (a -> Bool) -> [a] -> [Int]`
               - `findIndices (`elem` ['A'..'Z']) "Where Are Caps?"`
                  - returns `[0, 6, 10]`
            - zip/zipWith
               - `:t zip`
                  - `zip :: [a] -> [b] -> [(a, b)]`
               - `:t zipWith`
                  - `zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]`
               - zip/zipWith varients go up to 7
                  - zip, zip3, zip4, zip5, zip6, zip7
                  - zipWith, zipWith3, zipWith4, ...
               - `zipWith3 (\x y z -> x+y+z) [1, 2] [4, 5] [2, 2]`
                  - returns `[7, 9]`
            - lines
               - returns every line of a string in a list
            - unlines
               - takes list of strings, returns 1 string with lines
            - words
               - splits word string into list of strings
            - unwords
               - takes list and returns 1 string with spaces
            - nub
               - takes list, weeds out duplicates
            - delete
               - takes element and list, and deletes
                 first occurance of element
               - `delete 'h' "hey there"`
                  - returns: `"ey there"`
            - `\\` (list difference)
               - removes first occurance of every video
               - similar to set difference
               - `[1..10] \\ [2, 5, 9]`
                  - returns `[1, 3, 4, 6, 8, 10]`
            - union
               - appends stuff from second list into first
               - duplicates get removed
               - ```"hey man" `union` "what's up"`
                  - returns `"key manwt'sup"`
            - intersect
               - takes 2 lists and returns elements in common
            - insert
               - inserts element into list
               - searches until finds element that's equal or
                 greater than item being inserted and inserts
                 just before that element
               - if inserted in sorted list, result will stay sorted
               - `:t insert`
                  - `insert :: (Ord a) => a -> [a] -> [a]`
               - `insert 4 [3,5,1,2,8,2]`
                  - returns `[3,4,5,1,2,8,2]`
               - `insert 4 [1,3,4,4,1]`
                  - returns `[1,3,4,4,4,1]`



      - Control.Monad
         - functions
            - when
               - takes a boolean and I/O action and
                 if Bool is True, returns I/O action
               - if False, returns ()
               - same as: if x then do I/O else return ()
               - ex: `when Bool do blah <- test`
            - forever
               - takes I/O action & returns I/O
                 action that repeats it forever
            - forM
               - similar to mapM, but reversed
                 parameter order
               - make I/O action for every list element

      - System.Directory
         - openTempFile
            - takes directory path, and template name of file
            - returns tuple with actual file name, and file handle
            - actual file name = template name + some characters
            - `:t openTempFile :: FilePath -> String -> IO (FilePath, Handle)`
            - `openTempFile "." "temp"`
         - doesFileExist
            - `:t doesFileExist :: FilePath -> IO Bool`
         - removeFile
            - `:t removeFile :: FilePath -> IO ()`
         - renameFile
            - `:t renameFile :: FilePath -> IO ()`
         - getCurrentDirectory
            - `:t getCurrentDirectory :: IO FilePath`
         - copyFile
            - TODO

      - System.Environment
         - getArgs
            - `:t getArgs :: IO [String]`
            - cmd arguments
            - double quotes for multi-word arg as 1 arg:
               - ex: `"fdsf sdfsd fds"`
         - getProgName
            - `:t getProgName :: IO String`
            - executable name

      - `Control.Exception` (old name `System.IO.Error`)
         - catch
            - old type signature
               - `catch :: IO a -> (IOError -> IO a) -> IO a`
            - new type signature
               - `catch :: Exception e => IO a -> (e -> IO a) -> IO a`

            - takes IO action that can fail
            - second is handler that takes exception


      - `System.IO.Error`
         - functions
            - `ioError :: IOException -> IO a`
               - takes I/O error and produces I/O action that will throw the error
               - can act as IO anything because it doesn't yield a result
            - `userError`
               - makes an exception; caught with isUserError
               - ex: `ioError $ userError "test"`
            - `isDoesNotExistError :: IOError -> Bool`
               - predicate that takes `IOError` and returns true if file doesn't exist
            - `isAlreadyExistsError`
            - `isAlreadyInUseError`
            - `isFullError`
            - `isEOFError`
            - `isIllegalOperation`
            - `isPermissionError`
            - `isUserError`
               - checks if exception was made by `userError` function
            - `ioeGetFileName :: IOError -> Maybe FilePath`
               - if exception has filepath, extract it


      - System.IO

         - types
            - FilePath
               - `type FilePath = String`
            - IOMode
               - `data IOMode = ReadMode | WriteMode | AppendMode | ReadWriteMode`

         - functions
            - withFile
               - `:t withFile :: FilePath -> IOMode -> (Handle -> IO a) -> IO a`
               - ```withFile "test.txt" ReadMode (\handle -> do
                       contents <- hGetContents handle
                       putStr contents)```
            - openFile
               - `handle <- openFile "fname.txt" ReadMode`
               - `:t openFile :: FilePath -> IOMode -> IO Handle`
            - hClose
               - `hClose handle`
            - hGetContents
               - `contents <- hGetContents handle`
            - hGetLine/hPutStr/hPutStrLn/hGetChar
               - file equivalent of std in/out
            - readFile
               - read file and return IO String
               - `:t readFile :: FilePath -> IO String`
            - writeFile
               - takes path and string, writes to file
               - if file already exists, overwrites
               - `:t writeFile :: FilePath -> String -> IO ()`
            - appendFile
               - same as writeFile, but append mode
            - hSetBuffering
               - takes handle and BufferMode, and returns I/O to set BufferMode
               - `:t hSetBuffering -> Handle -> BufferMode -> IO ()`
            - hFlush
               - takes handle and flushes buffer




- Typeclasses 2
   - notes
      - defines some behavior and types that can behave in
        that way are made instances
      - multiple typclass constraints in signature
         - `f :: (Num a, Eq a) => a -> a`

   - vocab
      - concrete type = fully applied type without parameters
      - type constructor
         -takes argument to make concrete type

   - possible things to derive in ADT
      - Eq, Ord, Show, Read
      - Bounded
         - `minBound :: Int`
            - returns lowest possible integer
         - `maxBound :: Int`
            - maximum integer
      - Enum
         - `succ 1`
            - returns next
         - `pred 1`
            - returns previous
         - `[1..5]`
            - returns range
      - Eq/Ord
         - `==`, `>`
         - Ord: `compare`
      - Read
         - `read "Person {firstName =\"Michael\",lastName =\"Diamond\", age = 43}" :: Person`
            - returns Person as an Object


   - ADT
      - `data Maybe a = Nothing | Just a`
      - `:k`
         - can be used on any time
         - `:k Maybe`
            - returns: `Maybe :: * -> *`
         - `:k Int`
            - `Int :: *`
         - `:k Num`
            - `Num :: * -> GHC.Prim.Constraint`

   - Making typeclasses
      - doesn't need implement function bodies, but
      need to make define their type signatures


- IO
   - compiling single file
      - `ghc --make filenoextname`
      - don't add .hs

   - misc notes
      - any I/O code is tained
         - so are computation depending on tained I/O
      - IO typeclass
         - tainted
      - main
         - IO action performed when in main

      - do
         - glues several IO actions into one
         - last action cannot be bound to a name
         - last action is returned from do


      - `<-`
         - store variable
         - extracts value from IO box
         - temporary untaints the code
         - `name <- getLine`
            - perform I/O computation and
              bind result to name


   - misc I/O functions

      - return
         - makes IO action out of pure value
         - `:t return`
            - `return :: Monad m => a -> m a`
         - `let x = return 5`
            - `:t x`
               - `x :: (Monad m, Num a) => m a

      - getLine
         - `:t getLine`
            - `getLine :: IO String`

      - putStrLn
         - takes a string and returns I/O action
           that has a result type ()
         - i.e. prints line to screen
         - `:t putStrLn`
            - `putStrLn :: String -> IO ()`
         - `:t putStrLn "Hello"`
            - `putStrLn "Hello" :: IO ()`

      - putStr
         - like putStrLn without new line

      - putChar
         - print out 1 character

      - print
         - takes value of any type that's instance
           of show, and prints it
         - `print = putStrLn . show`

      - getChar
         - `:t getChar`
            - `getChar :: IO Char`
         - due to buffering, doesn't start reading
           until user enters return

      - sequence
         - takes list of IO actions in sequence
         - result is a list of all IO results
         - `sequence $ map print [1,2,3]`
            - prints values, returns `[(), (), ()]`
         - fake `:t sequence`
            - `sequence :: [IO a] -> IO [a]`
         - real `:t sequence`
            - `sequence :: (Monad m, Traversable t) => t (m a) -> m (t a)`

      - mapM
         - takes a function and list, maps
           function, and then sequences it
         - `mapM print [1,2,3]`
            - prints values, returns `[(),(),()]`

      - mapM_
         - same as mapM, throws away return value


   - stream functions
      - getContents
         - reads from stdin until EOF
         - `:t getContents :: IO String`
         - lazy, used in piping
            - don't have to call multiple times
              for each line, reads line by line
      - interact
         - `:t interact :: (String -> String) -> IO ()`
         - takes a string manipulation function,
           and returns I/O that takes input and
           runs function on input





- Exceptions
   -modules
      - `Control.Exception` for `catch`
      - `System.IO.Error` for I/O errors
      - `https://downloads.haskell.org/~ghc/6.10.1/docs/html/libraries/base/System-IO-Error.html#3`
   - pure code can throw exceptions
      - can only be caught in the I/O part of our code
      - bad practice to mix pure code and exceptions
         - use type system (Either/Maybe)
   - re-thrown uncaught exceptions


- Applicative Functors
   - fmap
      - `fmap :: Functor f => (a -> b) -> f a -> f b`
   - a functor is like a box
      - proper term computational context

   - to make type constructor instance of Functor, it needs kind `* -> *`
      - i.e. take 1 concrete type as type parameter
      - ex: `instance Functor Maybe where`
      - if takes 2 type parameters, we need to partially apply it
         - ex:
            - ```haskell
               instance Functor (Either a) where
                  fmap :: (b -> c) -> Either a b ->
                  Either a c
               ```

   - `IO`
      - `:t fmap`
         - `fmap :: (a -> b) -> IO a -> IO b`
      - if binding I/O result to name, only to call function, then use fmap instead
      - `IO String`
         - IO action, when performed gets a String

   - `(->) r a`
      - `:m + Control.Monad.Instances` to use
      - `:k (->)`
         - `(->) :: * -> * -> *`
      - `->` is just a type constructor that takes 2 parameters
         - `(->) r a`
      - partially apply to make functor: `(->) r`
      - implementation in `Control.Monad.Instances`
      - fmap
         - `fmap :: (a -> b) -> ((->) r a) -> ((->) r b)`
         - `fmap :: (a -> b) -> (r -> a) -> (r -> b)`

   - lifting
      - makes a function operate on functors
      - `let f = fmap (*3)`
         - `fmap (*3) :: (Num a, Functor f) => f a -> f a`
         - `f (Just 3)` returns `Just 9`
         - `f [100..102]` returns `[300, 303, 306]`
      - `let z = fmap (replicate 3)`
         - `z Just 3` returns `[Just 3, Just 3, Just 3]`
         - `z [1..3]` returns `[[1,1,1],[2,2,2],[3,3,3]]`
         - `z $ Right "blah"` returns `Right ["black", "blah", "blah"]`
         - `z $ Left "hi"` returns `Left "hi"`
      - currying
         - function that takes `a` and returns `b -> c`
            - `a -> b -> c`
            - `a -> (b -> c)`
      - normal sig: `fmap :: ((a -> b) -> f a) -> f b`
         - function that takes 1 function and functor, and returns functor
      - lifting sig: `fmap :: (a -> b) -> (f a -> f b)`
         - function that takes functor and returns a new function that's
           like old one, but it takes a functor as parameter and returns functor

   - functor laws
      - 1st: if map id over functor, the functor should be same as original functor
         - fmap id over a functor, it should besame as calling id on functor
         - i.e. `fmap id' == `id`
         - `fmap id (Just 3)` == `id $ Just 3` returns `Just 3`
         - `fmap id [1..5]` == `id [1..5]`


      - 2nd: composing 2 functions and then mapping result function over a functor should be the same
        as first mapping one function over the functor and then mapping the other one
         - `fmap (f . g)` == `fmap f . fmap g`
         - for any functor F, following holds
            - `fmap (f . g) F` == `fmap f (fmap g F)`



   - Applicative functors
      - `Control.Applicative` module

      - Applicative Laws
         - `pure id <*> v` = `v`
         - `pure (.) <*> u <*> v <*> w` = `u <*> (v <*> w)`
         - `pure f <*> pure x` = `pure (f x)`
         - `u <*> pure y` = `pure ($ y) <*> u`

      - by mapping multi-parameter functions over functor, we get a functors that contain functions
         - ex1: `fmap (*) (Just 3)`
            - returns `Just ((*) 3)`
         - ex2 usage:
            - `let a = fmap (*) [1,2,3,4]`
               - `:t a :: [Integer -> Integer]`
            - `fmap (\f -> f 9) a`
               - returns `[9, 8, 27, 36]`


      - applicative usage (why)
         - normal fmap maps a function over functor
         - we want to map a function inside functor over functor
            - ex1: 2 functors `Just (3 *)` and `Just 5`
               - returns: `Just $ 3*15`
         - takes parameters that aren't necessarily wrapped in functor, and operates on values in functor contexts
            - alternative to `pure f <*> x <*> y <*> ...`


      - Applicative typeclass functions
         - `pure : a -> f a`
         - `(<*>) :: f (a -> b) -> f a -> f b`
         - `(<$>)`
            - fmap as infix operator
            - `(<$>) :: (Functor f) => (a -> b) -> f a -> f b`
            - `f <$> x = fmap f x`


      - Functors & Applicatives and `<$>`
         - `<$>` is fmap as infix operator
         - `f <*> x` = `fmap f x`
         - `pure f <*> x` = `fmap f x`
            - pure puts values in default context
         - `f <$> x <*> y <*> z` to apply f between three applicative functors
            - if arguments weren't applicative, `f x y z`
         - alternatives
            - v1: `pure f <*> x <*> y <*> ...`
            - v2: `fmap f x <*> y <*> ...`
            - v3: `f <$> x <*> y <*> z`

      - Just examples
         - ex1: `pure (+3) <*> Just 5`
            - returns: `Just 8`
         - ex2: `let x = pure (+3) <*> pure 5`
            - `x`
               - returns `8`
            - `:t x`
               - `x :: (Num b, Applicative f) => f b`
            - `fmap (+3) x`
               - can't do `x + 5`
         - ex3: `Just (+3) <*> Nothing` or `pure (+3) <*> Nothing`
            - returns `Nothing`
         - ex4: `pure (+) <*> Just 3 <*> Just 5`
            - returns: `Just 8`
         - ex5: `pure (+) <*> Just 3 <*> Nothing`
            - returns `Nothing`

      - `<$>` examples:
         - ex1: `(+3) <$> pure 3`
         - ex2: `(+3) <$> (+3) <$> Just 5`
         - ex3: `(++) <$> Just "John" <*> Just "Smith"`
         - ex4: `(+3) <$> (Just (+5) <*> (pure (+5) <*> Just 3))`
            - returns `Just 16`
         - ex5: `(+3) <$> (Just (+5) <*> ((+5) <$> pure 3))`
         - ex6: `let sum3 a b = (+) (a+b)`
            - `sum3 1 2 3`
               - returns `6`
            - `sum3 <$> pure 1 <*> Just 2 <*> pure 3`
               - returns `Just 6`


      - list examples:
         - `(+3) <$> [1,2,3]`
            - returns `[4,5,6]`
         - `(+) <$> [1,2,3] <*> [3,2,1]`
            - returns `[4,3,2,5,4,3,6,5,4]`
            - breakdown
               - `:t ((+) <$> [1,2,3] <*>)`
                  - `((<*>) ((<$>) (+) [1,2,3])) :: Num b -> [b] -> [b]`
               - same as `(map (+) [1,2,3]) <*> [3,2,1]`
               - same as `[f x | f <- (map (+) [1,2,3]), x <- [3,2,1]]`
         - `pure (+3) <*> [1,2,3]`
            - returns `[4,5,6]`
            - same as `[(+3)] <*> [1,2,3]`
            - same as `(+3) <$> [1,2,3]`
         - `pure 3 :: [Int]`
            - returns `[3]`
         - `[(*0), (+100), (^2)] <*> [1,2,3]`
            - returns `[0,0,0,101,102,103,1,4,9]`
         - `[(+), (*)] <*> [1,2] <*> [3,4]`
            - returns `[4,5,5,6,3,4,6,8]`
            - same as `[(+)1, (*)1, (+)2, (*2)] <*> [3,4]`
         - `(*) <$> [2,5,10] <*> [8,10,11]`
            - same as `[x*y | x <- [2,5,10], y<-[8,10,11]]`
         - same as above, but filter
            - `filter (>50) $ (*) <$> [2,5,10] <*> [8,10,11]`

      - IO Applicative
         - `<*>` specialized for `IO`
            - `(<*>) :: IO (a -> b) -> IO a -> IO b`

      - `Control.Applicative.ZipList` Applicative
         - ZipList is in Control.Applicative
         - examples:
            - `pure (+5) <*> (ZipList [1,2,3])`
               - result: `ZipList {getZipList = [6,8,9]}`
            - `getZipList $ (+) <$> ZipList [1,2,3] <*> ZipList [100,100,100]`
               - result: `[101, 102, 103]`
            -  `getZipList $ ZipList [(+3),(*2)] <*> ZipList [1,2]`
               - result `[1+3, 2*2]`

      - `Control.Applicative.liftA2`
         - applies function between 2 applicatives
         - type sig and defitinition:
            - `:t liftA2 :: (Applicative f) => (a -> b -> c) -> f a -> f b -> f c`
            - `:t liftA2 :: (Applicative f) => (a -> b -> c) -> (f a -> f b -> f c)`
            - `liftA2 f a b = f <$> a <*> b`
         - usage
            - ex1:
               - `getZipList $ liftA2 (+) (ZipList [1,2]) (ZipList [3,4])`
                  - result: `[4,6]`
            - ex2:
               - have `Just 3` and `Just 4`
               - need to get `Just [3,4]`
               - `liftA2 (:) (Just 3) (Just [4])`
               - `(:) <$> Just 3 <*> Just [4]`


      - `sequenceA`
         - transforms list of applicatives into applicative with a list
         - when using with I/O, `sequenceA` is same as `sequence`
            - takes a list of I/O actions and returns an I/O action that will
              perform all actions and have a IO list as result

         - definition
            - `sequenceA :: (Applicative f) => [f a] -> f [a]`
            - `sequenceA [] = pure []`
            - `sequenceA (x:xs) = (:) <$> x <*> sequenceA xs`

         - useful when a list of functions need to feed the same input to all of them, and view list of results
            - old way: `map (\f -> f 7) [(>4), (<10), odd]`
               - result: `[True, True, True]`
            - old way 2: `and $ map (\f -> f 7) [(>4), (<10), odd]`
               - result: True
            - new way: `sequenceA [(>4), (<10), odd] 7`
               - `:t` = `:: Integral a => a -> [Bool]`
               - result: `[True, True, True]`
               - turns list `(Num a) => [a -> Bool]` into `(Num a) => a -> [Bool]`

         - usage
            - ex1: `sequenceA [Just 3, Just 5]`
               - result: `Just [3,5]`
            - ex2: `sequenceA [Just 3, Nothing, Just 1]`
               - result: `Nothing`
            - ex3: `sequenceA [(+3), (+2), (+1)] 3`
               - result: `[6,5,4]`
            - ex4: `sequenceA [getLine, getLine, getLine]`



   - newtype keyword
      - wrap existing type in a new type
      - newtype can have only 1 value constructor and value constructor can only have one field
      - data keyword has overhead, newtype is faster
      - ex: `newtype ZipList a = ZipList { getZipList :: [a] }`
      - can use deriving keyword
      - using newtype to make type class instance
         - with maybe, it's simple
         - tuple instance of Functor
            - when fmap applied over tuple, it gets applied to first component
            - ```haskell
                  newtype Pair b a = Pair { getPair :: (a,b) }`
                  instance (Functor (Pair c) where
                     fmap f (Pair (x,y)) = Pair (f x, y)
               ```

      - newtype vs data
         - ```haskell
            helloMe :: CoolBool -> String
            helloMe (CoolBool _) = "hello"
            ```
         - ```haskell
            data CoolBool = CoolBool { getCoolBool :: Bool }
            helloMe undefined
            ```
            - returns `*** Exception: Prelude.undefined`
         - ``haskell
            newtype CoolBool = CoolBool { getCoolBool :: Bool }
            helloMe undefined
            ```
            - returns `"hello"`


      - `type` vs `newtype` vs `data`
         - `type`
            - `type` is for making type synonyms
               - `type IntList = [Int]`
            - can be used interchangeable with old type
            - no new constructor
            - makes type signatures more descriptive
         - `newtype`
            - taking existing types and wrapping them in new types
            - makes it easier to make types instances of certain type classes
            - cannot use interchangeably
            - record syntax gives a function for extracting value of original type
         - `data`
            - making new types
            - can have many constructors and fields


- Monoids
   - a monoid is when:
      - you have an associative binary function
      - a value which acts as identity with respect to that function

   - `Data.Monoid`
      - type class
         ```haskell
            class Monoid m where
               mempty :: m
               mappend :: m -> m -> m
               mconcat :: [m] -> m
               mconcat = foldr mappend mempty
            ```
         - `mempty`
            - not a function, but a polymorphic constant
      - `Product`
         - ex1: `getProduct $ mappend (Product 3) (Product 9)`
            - returns `27`
         - ex2: `getProduct $ mappend (Product 3) mempty`
            - returns `3`
         - ex3: `getProduct . mconcat . map Product $ [3,4,2]`
            - returns `24`
      - `Sum`
         - ex1: `getSum $ mappend (Sum 2) (Sum 9)`
            - returns 11
      - `Any`
         - ex1: `getAny $ mappend (Any True) (Any False)`
            - returns `True`
         - ex2: `getAny $ mappend mempty (Any True)`
            - returns `True`
         - ex3: `getAny . mconcat . map Any $ [False, False, False, True]`
            - returns `True`
         - ex4: `getAny $ mappend mempty mempty`
            - returns `False`
      - `All`
         - ex1: `getAll $ mappend mempty (All True)`
            - returns `True`
         - ex2: `getAll $ mappend mempty (All False)`
            - returns `False`
         - ex3: `getAll . mconcat . map All $ [True, True, True]`
            - returns `True`
         - ex4: `getAll . mconcat . map All $ [True, True, False]`
            - returns `False`
      - `Ordering` Monoid
         - compare things based on different criteria
            - put the criteria in an order
      - `Maybe` and `First` Monoid
         - method 1 (default, `Maybe a` where `a` is a Monoid)
            - ex1: `mappend (Just [5]) (Just [3])`
               - returns: `Just [5,3]`
            - ex2: `mappend (Just $ Sum 3) (Just $ Sum 4)`
               - returns `Just (Sum {getSum = 7})`
         - method 2 (`First`, returns first value):
            - ex1: `mappend (First $ Just 5) (First $ Just 65)`
               - returns: `First {getFirst = Just 5}`
            - ex2: `getFirst . mconcat . map First $ [Nothing, Just 9, Just 10]`
               - returns: `Just 9`
         - method 3 (`Last`):
            - ex1: `getLast . mconcat . map Last $ [Nothing, Just 9, Just 10]`
               - returns `Just 10`
            - ex2: `getLast $ mappend (Last $ Just "one") (Last $ Just "two")`
               - returns `Just "two"`



   - `Data.Foldable`
      - `import qualified Data.Foldable as F`
      - `Foldable` type class
         - `foldMap :: (Monoid m, Foldable t) => (a -> m) -> t a -> m`
            - if this function is implemented, our type becomes instance of `Foldable`
            - first parameter is a func that
               - takes value of type that our foldable structure contains
               - returns a monoid value
            - second parameter is a foldable structure that contains values of type `a`
            - maps function over the foldable struct, producing a foldable struct that contains monoid values
               - then does `mappend` between those monoid values

      - misc
         - has `foldr`, `foldl`, `foldr1`, `foldl1`
         - difference with normal fold
            - `:t foldr :: (a -> b -> b) -> b -> [a] -> b`
            - `:t F.foldr :: (F.Foldable t) => (a -> b -> b) -> b -> t a -> b`


   - Monoid laws
      - `mappend mempty x` = `x`
      - `mappend x mempty` = `x`
      - `mappend (mappend x y) z` = `mappend x (mappend y z)`

   - `*` with `1` and `++` with `[]` shared properties
      - function takes 2 parameters
      - parameters and return value have the same type
      - there exists a value that doesn't change other values when used with the binary function
         - `1` is identity with respect to `*`
         - `[]` is identity with respect to `++`
      - associative
         - when we have 3+ values, and we want to reduce them to single value
            - the order we apply binary function doesn't matter
         - `(3*2)*(8*5)` = `3*(2*(8*5))`



- Monads

- TODO:
   - implement foldl1 foldr1, scans, etc


