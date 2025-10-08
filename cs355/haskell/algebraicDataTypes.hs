data Shape = Circle Float | Rectangle Float Float

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
