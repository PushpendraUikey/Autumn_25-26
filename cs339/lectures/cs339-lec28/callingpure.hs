name2reply :: String -> String
name2reply name =
    "Pleased to meet you, " ++ name ++ ".\n" ++ "Your name contains " ++ charcount ++ " characters."
    where charcount = show (length name)

eval :: Expr -> Maybe Int 
eval (Val n) = Just n 
eval (Div x y) = case eval x of 
                    Nothing -> Nothing
                    Just n -> case eval y of
                        Nothing -> Nothing
                        Just m -> safediv n m
-- So far I've seen this arrow (->) in three places only
-- first in lambda return expression, second when defining types
-- and third here to delimit the pattern and return value.

main :: IO ()
main = do
        putStrLn "Greetings once again. What is your name?"
        inpStr <- getLine
        let outStr = name2reply inpStr
        putStrLn outStr
