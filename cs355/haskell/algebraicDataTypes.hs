-- data Shape = Circle Float | Rectangle Float Float

area :: Shape -> Float
area (Circle r)        = pi * r^2
area (Rectangle l b)   = l * b


-- data Maybe a = Nothing | Just a 
safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (x:_) = Just x

safeDivision :: Int -> Int -> Maybe Int 
safeDivision _ 0 = Nothing
safeDivision m n = Just (m `div` n)


-- Recursive Types
data Nat = Zero | Succ Nat  -- Church Numerals

nat2int :: Nat -> Int
nat2int Zero = 0
nat2int (Succ n) = 1 + nat2int n

data Shape = Circle Float | Rectangle Float Float

instance Eq Shape where
    (==) :: Shape -> Shape -> Bool
    (Circle r1) == (Circle r2) = r1 == r2
    (Rectangle l1 b1) == (Rectangle l2 b2) = l1 == l2 && b1 == b2
    _ == _ = False

-- class (Eq a, Show a) => Num a where 
--   (+),(-),(*) :: a -> a -> a 
--   negate :: a -> a 
--   abs, signum :: a -> a  
--   fromInteger :: Integer -> a 


-- data Bool = False | True
-- instance Eq Bool where 
--     False == False = True 
--     True  == True  = True 
--     _     == _     = False

-- This deriving method generates a definition; the "instance" one does not.
-- data Student = Student { name :: String,
--                          rollNum :: Int} deriving Show 

data Student  = Student { name :: String, 
                          rollNum :: Int}
instance Show Student where
  show :: Student -> String
  show (Student n r) = "Student { name = " ++ show n ++ ", rollNum = " ++ show r ++ " }"
