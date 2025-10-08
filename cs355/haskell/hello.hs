--Haskell is a statically and strongly typed language, meaning every expression has a 
--type known at compile time
main :: IO ()
main = do
    putStrLn "Hello everybody!"
    putStrLn ("Please look at my favorite odd numbers: " ++ show (filter odd [10..20]))
    -- Message and list are concatenated using ++