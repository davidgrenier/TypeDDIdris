module Ex3

import Data.Vect

myLength : List a -> Nat
myLength [] = 0
myLength (x :: xs) = S (myLength xs)

myReverse : List a -> List a
myReverse [] = []
myReverse (x :: xs) = myReverse xs ++ [x]
-- myReverse xs =
--     rev [] xs
--     where
--         rev : List a -> List a -> List a
--         rev xs [] = xs
--         rev xs (x :: ys) = rev (x :: xs) ys

myMap : (a -> b) -> List a -> List b
myMap f [] = []
myMap f (x :: xs) = f x :: myMap f xs

vMap : (a -> b) -> Vect n a -> Vect n b
vMap f [] = []
vMap f (x :: xs) = f x :: vMap f xs

{-
myLength [1..10]
myReverse [1..10]
myMap (* 2) [1..10]
vMap length ["Hot", "Dog", "Jumping", "Frog"]
-}
