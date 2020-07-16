module Modules

import Data.List
import Data.Strings

export
average : String -> Double
average text =
    let words = words text
        lengths = map length $ words in
    cast (sum lengths) / cast (length words)
