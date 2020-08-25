module Ex2

import Data.List
import Data.Strings
import System.REPL

palindrome : String -> Bool
palindrome str = reverse str == str

palindInsensitive : String -> Bool
palindInsensitive = palindrome . toLower

palinLength : Nat -> String -> Bool
palinLength bound str = length str > bound && palindrome str

palinLong : String -> Bool
palinLong = palinLength 10

counts : String -> (Nat, Nat)
counts str = (length (words str), length str)

topTen : Ord a => List a -> List a
topTen = take 10 . reverse . sort

overLength : Nat -> List String -> Nat
overLength minLen = length . filter ((> minLen) . length)

main : IO ()
main =
    repl "Enter a potential palindrome> " loop
    where
        loop : String -> String
        loop str =
            let pref = if palindrome str then "A" else "Not a" in
            pref ++ " palindrome of structure " ++ show (counts str) ++ "\n"
