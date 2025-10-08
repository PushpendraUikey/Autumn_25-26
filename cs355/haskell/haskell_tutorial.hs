import GHC.Num (integerShiftL)
import GHC.Integer.GMP.Internals (powModInteger)
import Prelude hiding (Word)
import Data.Char (toLower)
x :: Int
x = 3

-- x = 4
{-
"="" does not denote “assignment” like it does in many other languages. Instead, "="
denotes definition, like it does in mathematics. That is, x = 4 should not be read 
as “x gets 4” or “assign 4 to x”, but as “x is defined to be 4”
-}

i :: Int
i = -78

biggestInt, smallestInt :: Int
biggestInt = maxBound
smallestInt = minBound

-- Arbitrary precision integerShiftL
n, reallyBig :: Integer
n = 7193498193479174397013987491734978190739749327149710983470918354439857490123483249723
reallyBig = 2^(2^(2^(2^2)))

numDigits :: Int
numDigits = length (show reallyBig)

-- Double precision floating point 
d1, d2 :: Double
d1 = 4.5523
d2 = 6.23409e-4
-- There is also a single precision floating point number type :: Float 

-- Booleans 
b1, b2 :: Bool
b1 = True
b2 = False

-- Unicode Characteers 
c1, c2, c3 :: Char
c1 = 'x'
c2 = 'Ø'
c3 = 'ダ'

-- String 
s :: String
s = "Hello, Haskell!"


-- badArith1 = i + n 
-- Addition is only between values of the same numeric type, and Haskell does not 
-- do implicit conversion

-- badArith2 = i / i 
-- this is an error since / performs floating-point division only. For integer division
-- we use `div`

{-
Haskell also has if-expressions: if b then t else f is an expression which evaluates 
to t if the Boolean expression b evaluates to True, and f if b evaluates to False. 
Notice that if-expressions are very different than if-statements. For example, 
with an if-statement, the else part can be optional; an omitted else clause 
means “if the test evaluates to False then do nothing”. With an if-expression, on the 
other hand, the else part is required, since the if-expression must result in some value.
-}

-- Compute Sum of integers from 1 to n 
sumtorial :: Integer -> Integer
sumtorial 0 = 0
sumtorial n = n + sumtorial (n-1)

-- Choices can also be made based on arbitrary Boolean expressions using guards 
hailstone :: Integer -> Integer
hailstone n
    | even n = n `div` 2
    | otherwise      = 3*n + 1

type Word = [Char]
type Text = [Char]

-- words :: Text -> [Word]
-- words = Prelude.words
-- map toLower :: Text -> Text , we need this to count Uppercase and lowercase same 
sortWords :: [Word] -> [Word]
countRuns :: [Word] -> [(Int, Word)] -- Count adjacent runs for each word
countRuns [] = []
countRuns (w:ws) = (1+length us, w) : countRuns vs
                    where (us, vs) = span (==w) ws
-- length :: [a] -> Int
-- length [] = 0
-- length (_:xs)  = 1 + length xs
-- span :: (a->Bool) -> [a] -> ([a], [a])
-- span p [] = []
-- span p (x:xs) = if p x then (x:ys, zs)
--                 else ([], x:xs)
--                 where (ys, zs) = span p xs 

sortRuns :: [(Int, Word)] -> [(Int, Word)]
showRun :: (Int, Word) -> String
showRun (n, w) = w ++ ": " ++   -- The ++ is concatenation of strings: (++) expects two strings 
                    show n ++ "\n" -- To convert numerals to string we use show
-- pair is taken as (first, second) inside paranthesis

-- map showRun :: [(Int, Word)] -> [String] -- This will be used to show all the words
-- concat :: [[a]] -> [a]

f :: Num a => a -> a
f x = x + 1
g :: Num a => a -> a
g y = y * y
-- Function composition . has lower precedence than function application.

-- Checking if the list is empty:
null :: [a] -> Bool
null [] = True
null _ = False

-- map :: (a->b) -> [a] -> [b]
-- map f [] = []
-- map f (x:xs) = f x : map f xs
-- map f xs = [f x | x <- xs]  -- Map using list comprehension

-- filter :: (a -> Bool) -> [a] -> [a]
-- filter p [] = []
-- filter p (x:xs) = if p x
--                     then x : filter p xs    -- cons -ing, x is car and xs is cdr
--                     else filter p xs
-- filter p xs = [x | x <- xs, p x]

-- concatenation
-- concat xss = [x | xs <- xss, x <- xs]

isPrime :: Int -> Bool
isPrime n = checkPrime n 2
checkPrime :: Int -> Int -> Bool
checkPrime n k
    | k * k > n      = True                 -- no divisor ≤ √n found → prime
    | n `mod` k == 0 = False                -- divisible → not prime
    | otherwise      = checkPrime n (k + 1) -- test next k


sort :: Ord a => [a] -> [a]
sort [] = []
sort [x] = [x]
sort xs  = merge (sort ys) (sort zs)
           where (ys, zs) = half xs
half :: [a] -> ([a], [a])
half xs = splitAt n xs
           where n = length xs `div` 2

merge :: Ord a => [a] -> [a] -> [a]
merge [] ys = ys
merge xs [] = xs
merge (x:xs) (y:ys)
    | x <= y = x : merge xs (y:ys)
    | otherwise = y : merge (x:xs) ys

sortWords = sort
sortRuns  = reverse . sort

commonWords n = concat . map showRun . take n . sortRuns . countRuns . sortWords .
                words . map toLower