module Loops

import System.REPL
import Modules

showAverage : String -> String
showAverage str =
    "The average word length is: " ++
    show (average str) ++ "\n"

main : IO ()
main = repl "> " showAverage
