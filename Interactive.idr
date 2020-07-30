module Interactive

import Utils.Prelude
import System.REPL
import Data.Strings

-- test : IO ()
-- test = do
--     x <- ?action
--     ?more x

main : IO ()
main = do
    putStrLn "Enter something"
    line <- getLine
    let len = length line
    show len |> putStrLn

-- main : IO ()
-- main =
--     putStrLn "Enter something"
--     >>= \_ => getLine
--     >>= \line => (putStrLn $ show $ length line)

printLonger : IO ()
printLonger = do
    putStrLn "Write two lines"
    line1 <- getLine
    line2 <- getLine
    let len = length (line1 ++ line2)
    putStrLn (show len)

printLong : IO ()
printLong =
    putStrLn "First String: "
    >>= \_ => getLine
    >>= \line1 => putStrLn "Second String: "
    >>= \_ => getLine
    >>= \line2 => length (line1 ++ line2) |> show |> putStrLn

measuring : IO ()
measuring = do
    putStrLn "Type a word"
    getLine >>=
        length
        >> show
        >> ("The word has length " ++)
        >> putStrLn
    measuring

recur : Nat -> IO ()
recur 0 = do
    putStrLn "Type a number"
    getLine >>=
        cast
        >> fromInteger
        >> recur
recur (S k) =
    printLn (S k)
    >>= \_ => recur k

allDigits : String -> Bool
allDigits = unpack >> all isDigit

readNumber : IO (Maybe Nat)
readNumber = do
    input <- getLine
    if allDigits input
        then pure $ Just $ stringToNatOrZ input
        else pure Nothing

test : IO (Maybe (Nat, Nat))
test = do
    Just n1 <- readNumber | Nothing => pure Nothing
    Just n2 <- readNumber | Nothing => pure Nothing
    pure (Just (n1, n2))
