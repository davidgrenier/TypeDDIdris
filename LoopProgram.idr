module LoopProgram

import Utils.Prelude
import Data.Strings
import System

countdown : Nat -> IO ()
countdown Z = putStrLn "Lift off!"
countdown t@(S secs) = do
    printLn t
    usleep 1000000
    countdown secs

readNumber : IO (Maybe Nat)
readNumber = do
    line <- getLine
    if all isDigit (unpack line)
        then pure $ Just $ stringToNatOrZ line
        else pure Nothing

countdowns : IO ()
countdowns = do
    putStr "Enter starting number: "
    Just num <- readNumber
        | Nothing => do
            putStrLn "Invalid input"
            countdowns
    countdown num
    putStr "Another (y/n)? "
    "y" <- getLine
        | _ => pure ()
    countdowns
