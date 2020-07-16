module Whitespace

import Data.Strings
import Data.List

average : String -> Double
average text =
    let numWords = wordCount text
        totalLength = sum (allLengths (words text)) in
    cast totalLength / cast numWords
    where
        allLengths : List String -> List Nat
        wordCount : String -> Nat
