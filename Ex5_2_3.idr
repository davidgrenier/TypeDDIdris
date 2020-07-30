module Ex5_2_3

import System
import Data.Strings
import Data.Fin
import System.REPL

askInteger : Nat -> IO Integer
askInteger max = do
    line <- getLine
    if all isDigit (unpack line)
        then pure $ cast line
        else do
            putStr "The input must consist only of digits: "
            askInteger max

askFinite : (max : Nat) -> IO (Maybe (Fin max))
askFinite max = do
    0 <- askInteger max
        | number => pure $ integerToFin (number - 1) max
    putStr "The guess must be strictly positive: "
    askFinite max

getValidGuess : Nat -> IO Nat
getValidGuess max = do
    Just valid <- askFinite max
        | Nothing => do
            putStr ("The number must be less than " ++ show max ++ ": ")
            getValidGuess max
    pure $ finToNat valid

genNumber : Nat -> IO Nat
genNumber max = do
    t <- time
    let num = integerToFin (mod t (cast max)) max
    Just num <- pure num
        | Nothing => pure 0
    pure (finToNat num)

trySolve : Nat -> Nat -> IO Nat -> IO ()
trySolve try target fetcher = do
    num <- fetcher
    if num == target
        then putStrLn ("You found it after " ++ show try ++ " tries.")
        else do
            let message = if num < target then "low" else "high"
            putStr ("Your guess is too " ++  message ++ ", try again (" ++ show (try + 1) ++ "): ")
            trySolve (try + 1) target fetcher

main : IO ()
main = do
    let max = 100
    target <- genNumber max
    putStr ("Guess a number between 1 and " ++ show max ++ ": ")
    trySolve 1 target (getValidGuess max)

myRepl : String -> (String -> String) -> IO ()
myRepl x f = do
    putStr x
    input <- getLine
    putStr $ f input
    myRepl x f

myReplWith : a -> String -> (a -> String -> Maybe (String, a)) -> IO ()
myReplWith state init f = do
    putStr init
    input <- getLine
    Just (output, newState) <- pure $ f state input
        | Nothing => pure ()
    putStr output
    myReplWith newState init f
