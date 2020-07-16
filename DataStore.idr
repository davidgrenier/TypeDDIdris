module Main

import Data.Vect
import System.REPL
import Data.Strings

infixl 5 |>
infixl 5 >>
%inline
(|>) : a -> (a -> b) -> b
(|>) x f = f x

%inline
(>>) : (a -> b) -> (b -> c) -> a -> c
(>>) f g x = g (f x)

data DataStore : Type where
    Create : (n : Nat ) -> Vect n String -> DataStore

size : DataStore -> Nat
size (Create n _) = n

items : (store : DataStore) -> Vect (size store) String
items (Create _ xs) = xs

addToStore : String -> DataStore -> DataStore
addToStore item store =
    items store
    |> append
    |> Create _
    where
        append : Vect n String -> Vect (S n) String
        append [] = [item]
        append (y :: ys) = y :: append ys

data Command
    = Add String
    | Get Integer
    | Quit

parseCommand : String -> String -> Maybe Command
parseCommand "quit" _ = Just Quit
parseCommand "add" text = Just (Add text)
parseCommand "get" id =
    if all isDigit (unpack id)
    then Just $ Get (cast id)
    else Nothing
parseCommand _ _ = Nothing

mapSnd : (b -> c) -> (a,b) -> (a,c)
mapSnd f (x, y) = (x, f y)

parse : String -> Maybe Command
parse =
    span (/= ' ')
    >> mapSnd ltrim
    >> uncurry parseCommand

getEntry : DataStore -> Integer -> Maybe (String, DataStore)
getEntry store id =
    case integerToFin id (size store) of
    Nothing => Just ("Out of range\n", store)
    Just entry  => Just (index entry (items store) ++ "\n", store)

loop : DataStore -> String -> Maybe (String, DataStore)
loop store input =
    case parse input of
    Nothing => Just ("Invalid command\n", store)
    Just Quit => Nothing
    Just (Get id) => getEntry store id
    Just (Add item) =>
        Just ("ID " ++ show (size store) ++ "\n", addToStore item store)

main : IO ()
main = replWith (Create _ []) "Command: " loop
