module Ex5_3_4

import Utils.Prelude
import Data.Vect
import Data.Strings
import System.File
import Data.Either

-- openFile : String -> Mode -> IO (Either FileError File)
-- closeFile : File -> IO ()
-- fEOF : File -> IO Bool
-- fGetLine : File -> IO (Either FileError String)
-- writeFile : String -> String -> IO (Either FileError ())
-- data Mode = Read | WriteTruncate | Append | ReadWrite | ReadWriteTruncate | ReadAppend

readToBlank : IO (List String)
readToBlank = do
    "" <- getLine
        | line => do
            rest <- readToBlank
            pure (line :: rest)
    pure []

readAndSave : IO ()
readAndSave = do
    putStrLn "Enter a sequence of lines until a blank line:"
    content <- readToBlank
    putStr "Type a file name for the content: "
    filename <- getLine
    -- Right file <- openFile filename ReadWrite
    --     | Left error => printLn error
    Right () <-
            map (++ "\n") content
            |> concat
            |> writeFile filename
        | Left error => printLn error
    pure ()

readVectFile : (filename : String) -> IO (n ** Vect n String)
readVectFile filename = do
    Right file <- openFile filename Read
        | Left error => do
            printLn error
            pure (_ ** [])
    read file
    where
        read : File -> IO (n ** Vect n String)
        read file = do
            False <- fEOF file
                | True => pure (_ ** [])
            Right line <- fGetLine file
                | Left error => do
                    printLn error
                    pure (_ ** [])
            (_ ** rest) <- read file
            pure (_ ** line :: rest)
