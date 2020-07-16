module WordLength

import Data.Strings
import Data.Vect

allLengths : List String -> List Nat
allLengths [] = []
allLengths (word :: rest) = length word :: allLengths rest

lengths : List String -> List Nat
lengths strings =
    lengths strings []
    where
        lengths : List String -> List Nat -> List Nat
        lengths [] nums = nums
        lengths (x :: xs) nums = lengths xs (length x :: nums)

xor : Bool -> Bool -> Bool
xor True = not
xor False = id

add : Nat -> Nat -> Nat
add Z j = j
add (S k) j = add k (S j)

mutual
    even : Nat -> Bool
    even Z = True
    even (S k) = odd k

    odd : Nat -> Bool
    odd Z = False
    odd (S k) = even k

{-
even 9
odd 9
even 8
odd 8
sum $ lengths $ words
sum $ allLengths $ words
xor False True
-}
