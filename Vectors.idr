import Data.Vect

tenInts : Vect 10 Int
tenInts = [0,1,2,3] ++ [4,5,6,7,8,9]

allLengths : Vect n String -> Vect n Nat
allLengths [] = []
allLengths (x :: xs) = length x :: allLengths xs

{-
sum $ allLengths $ replicate 1000 "hi"
-}
