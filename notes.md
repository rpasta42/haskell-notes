
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
      - prelude
         - `id`
            - takes parameter, returns same thing


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
            - `:t openTempFile :: FilePath -> String -> IO (FilePath, Handle)`

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




- TODO:
   - implement foldl1 foldr1, scans, etc


