module Ch9_2

import Data.Vect
import Decidable.Equality
import Utils.Prelude

data WordState : (guessesRemaining : Nat) -> (letters : Nat) -> Type where
    MkWordState : (word : String) ->
                    (missing : Vect letters Char) ->
                    WordState guessesRemaining letters

data Finished
    = Lost (WordState 0 (S letters))
    | Won (WordState (S guesses) 0)

data ValidInput : List Char -> Type where
    Letter : (c : _) -> ValidInput [c]

Uninhabited (ValidInput []) where
    uninhabited x impossible
Uninhabited (ValidInput (x::y::_)) where
    uninhabited x impossible

Show (ValidInput chars) where
    show (Letter c) = "Letter " ++ show c

isValidInput : (s : String) -> Dec (ValidInput (unpack s))
isValidInput s =
    isValid (unpack s)
    where
        isValid : (chars : List Char) -> Dec (ValidInput chars)
        isValid [] = No uninhabited
        isValid (c :: []) = Yes (Letter c)
        isValid (_ :: _ :: _) = No uninhabited

readGuess : IO (x ** ValidInput x)
readGuess = do
    putStr "Guess: "
    input <- getLine
    let (Yes prf) = isValidInput input
        | No contra => do
            putStrLn "Invalid guess"
            readGuess
    pure (_ ** prf)

removeElem : (x : a) -> (xs : Vect (S n) a) -> { auto proof : Elem x xs } -> Vect n a
removeElem x (x :: xs) { proof = Here } = xs
removeElem x (y :: []) { proof = There _ } impossible
removeElem x (y :: z :: xs) { proof = There _ } = y :: removeElem x (z :: xs)

processGuess : { letters : _ } -> (letter : Char) -> WordState (S guesses) (S letters) ->
                Either (WordState guesses (S letters))
                        (WordState (S guesses) letters)
processGuess letter (MkWordState word missing) =
    case isElem letter missing of
    No contra => Left (MkWordState word missing)
    Yes prf =>
        -- dropElem missing prf
        removeElem letter missing
        |> MkWordState word
        |> Right

formatHint : WordState (S guesses) (S letters) -> String
formatHint (MkWordState word missing) =
    unpack word |> format |> pack
    where
        format : List Char -> List Char
        format [] = []
        format (x :: xs) =
            case isElem (toLower x) missing of
            Yes prf => '_' :: format xs
            No contra => x :: format xs

game : { letters, guesses : _ } -> WordState (S guesses) (S letters) -> IO Finished
game state = do
    putStrLn ("Guess a letter of the word: " ++ formatHint state)
    (_ ** Letter c) <- readGuess
    case processGuess c state of
        Right s@(MkWordState _ []) => pure (Won s)
        Right s@(MkWordState _ (_::_)) => game s
        Left s =>
            case guesses of
            0 => pure (Lost s)
            S _ => game s

main : IO ()
main = do
    result <- MkWordState "Test" ['t','e','s'] |> game { guesses = 5 }
    case result of
        Won (MkWordState word _) => putStrLn ("You found the word: " ++ word)
        Lost (MkWordState word missing) => putStrLn ("You lose. The word was " ++ word)
