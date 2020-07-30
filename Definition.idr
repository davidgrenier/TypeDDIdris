module Definition

import Data.Strings
import Utils.Prelude

invert : Bool -> Bool
invert True = False
invert _ = True

describeList : Show a => List a -> String
describeList [] = "Empty"
describeList (x :: xs) = "Non-empty, tail = " ++ show xs

allLengths : List String -> List Nat
allLengths [] = []
allLengths (x::xs) = length x :: allLengths xs
