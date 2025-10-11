main = do       -- 'do' is syntatic sugar for performing chaining monadic actions!
        putStrLn ("Greetings! What is your name?") -- putStrLn is an action that prints
        inpStr <- getLine -- Runs the getLine action and takes the input
        putStrLn ("Welcome to APP, " ++ inpStr ++ "!")
-- do block Desugars as 
{-
main =
  putStrLn "Greetings! What is your name?" >>
  getLine >>= \inpStr ->
  putStrLn ("Welcome to APP, " ++ inpStr ++ "!")
-}
-- (>>=) :: IO a -> (a -> IO b) -> IO b
-- It runs the first action, extracts its result, and passes it to the next.

-- (>>) is a variant that ignores the result of the first action
-- (>>) :: IO a -> IO b -> IO b
-- (used for sequencing when there’s no <-).


{-
bind (>>= in Haskell) means:
If the previous computation succeeded, take its result and continue with this function;
if it failed, skip everything and propagate the failure.”

So it’s like a safe chain operator that propagates failure automatically.

Monads let you build pipelines of computations that carry context (like side effects, 
failure, state) — and automatically handle how that context flows.
-}