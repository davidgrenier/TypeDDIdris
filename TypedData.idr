module TypedData

import Utils.Prelude
import Data.Vect
import Data.List
import Data.Strings
import System.REPL

infixr 5 .+.

data Schema
    = SString
    | SInt
    | SChar
    | (.+.) Schema Schema

SchemaType : Schema -> Type
SchemaType SString = String
SchemaType SInt = Int
SchemaType SChar = Char
SchemaType (s1 .+. s2) = (SchemaType s1, SchemaType s2)

record DataStore where
    constructor Create
    schema : Schema
    size : Nat
    items : Vect size (SchemaType schema)

addToStore : (store : DataStore) -> SchemaType (schema store) -> DataStore
addToStore store item =
    items store
    |> append item
    |> Create _ _
    where
        append : a -> Vect n a -> Vect (S n) a
        append x [] = [x]
        append x (y :: ys) = y :: append x ys

getEntry : (store : DataStore) -> Nat -> Either String (SchemaType (schema store))
getEntry store id =
    case natToFin id (size store) of
    Nothing => Left "Out of range\n"
    Just id => Right (index id (items store))

data Command : Schema -> Type where
    SetSchema : (newschema : Schema) -> Command schema
    Add : (SchemaType schema) -> Command schema
    Get : Maybe Nat -> Command schema
    Size : Command schema
    Quit : Command schema

parsePrefix : (schema : Schema) -> List Char -> Maybe (SchemaType schema, List Char)
parsePrefix SChar [] = Nothing
parsePrefix SChar (x :: rest) = Just (x, rest)
parsePrefix SString ('"' :: rest) = do
    let (text, '"' :: rest) = span (/= '"') rest
        | _ => Nothing
    Just (pack text, rest)
parsePrefix SString _ = Nothing
parsePrefix SInt input =
    case input of
    '-' :: rest => getInt (-1) rest
    rest => getInt 1 rest
    where
        getInt : Int -> List Char -> Maybe (SchemaType SInt, List Char)
        getInt sign chars = do
            let ([], _) = span isDigit chars
                | (digits, rest) => Just (sign * cast (pack digits), rest)
            Nothing
parsePrefix (x .+. y) input = do
    (pre, ' ' :: rest) <- parsePrefix x input
        | _ => Nothing
    (post, _) <- parsePrefix y rest
        | _ => Nothing 
    Just ((pre, post), [])

parseBySchema : (schema: Schema) -> String -> Maybe (SchemaType schema)
parseBySchema schema input =
    case parsePrefix schema (unpack input) of
    Just (result, _) => Just result
    Nothing => Nothing

display : { schema : Schema } -> SchemaType schema -> String
display { schema = SString } x = show x
display { schema = SInt } x = show x
display { schema = SChar } x = show x
display { schema = y.+.z} (x, w) = display x ++ ", " ++ display w

parseSchema : List String -> Maybe Schema
parseSchema ["Char"] = Just SChar
parseSchema ["Int"] = Just SInt
parseSchema ["String"] = Just SString
parseSchema ("Char" :: xs) = parseSchema xs |> map (SChar .+.)
parseSchema ("Int" :: xs) = parseSchema xs |> map (SInt .+.)
parseSchema ("String" :: xs) = parseSchema xs |> map (SString .+.)
parseSchema _ = Nothing

parseCommand : (schema : Schema) -> String -> String -> Maybe (Command schema)
parseCommand _ "schema" schema = parseSchema (words schema) |> map SetSchema
parseCommand _ "quit" _ = Just Quit
parseCommand _ "size" _ = Just Size
parseCommand schema "add" text =
    parseBySchema schema text
    |> map Add
parseCommand _ "get" "" = Just $ Get Nothing
parseCommand _ "get" id =
    if all isDigit (unpack id)
    then Just $ Get $ Just (integerToNat $ cast id)
    else Nothing
parseCommand _ _ _ = Nothing

parse : (schema : Schema) -> String -> Maybe (Command schema)
parse schema command =
    let (op, rest) = span (/= ' ') command in
    parseCommand schema op (ltrim rest)

updateSchema : DataStore -> Schema -> Maybe DataStore
updateSchema (Create _ Z _) schema = Just (Create schema _ [])
updateSchema _ _ = Nothing

mapi : (Nat -> a -> b) -> Vect n a -> Vect n b
mapi f =
    mapi f 0
    where
        mapi : (Nat -> a -> b) -> Nat -> Vect j a -> Vect j b
        mapi f k [] = []
        mapi f k (x :: xs) = f k x :: mapi f (S k) xs

storeListing : DataStore -> String
storeListing store =
    items store
    |> mapi (\id, entry => show id ++ ": " ++ display entry ++ "\n")
    |> foldr (++) ""

processInput : DataStore -> String -> Maybe (String, DataStore)
processInput store input =
    case parse (schema store) input of
    Nothing => Just ("Invalid command\n", store)
    Just (SetSchema newSchema) =>
        case updateSchema store newSchema of
        Just store => Just ("Schema updated\n", store)
        Nothing => Just ("Entries would be lost\n", store)
    Just Quit => Nothing
    Just Size => Just ("DataStore has " ++ show (size store) ++ " entries\n", store)
    Just (Get Nothing) => Just (storeListing store, store)
    Just (Get (Just id)) =>
        case getEntry store id of
        Left message => Just (message, store)
        Right entry => Just (display entry ++ "\n", store)
    Just (Add item) =>
        Just ("ID " ++ show (size store) ++ "\n", addToStore store item)

main : IO ()
main = replWith (Create SString _ []) "Command: " processInput
