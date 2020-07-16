module Vect

import Data.Fin

infixl 5 |>
%inline
(|>) : a -> (a -> b) -> b
(|>) x f = f x

data Vect : Nat -> Type -> Type where
    Val : a -> Vect Z a
    (::) : (x : a) -> (xs : Vect m a) -> Vect (S m) a

%name Vect xs, ys, zs

append : Vect n a -> Vect m a -> Vect (n+m) a
append (Val x) ys = ys
append (x :: xs) ys = x :: append xs ys

zipWith : (a -> b -> c) -> Vect n a -> Vect n b -> Vect n c
zipWith f (Val x) (Val y) = Val (f x y)
zipWith f (x :: xs) (y :: ys) = f x y :: zipWith f xs ys

zip : Vect n a -> Vect n b -> Vect n (a,b)
zip = zipWith (\a,b => (a,b))

tail : Vect (S n) a -> Vect n a
tail (x :: xs) = xs

toList : Vect n a -> List a
toList (Val x) = [x]
toList (x :: xs) = x :: toList xs

rest : Vect n a -> List a
rest (Val x) = []
rest (x :: xs) = toList xs

length : { n : Nat } -> Vect n a -> Nat
length _ = n+1

index : Fin (S n) -> Vect n a -> a
index FZ (Val x) = x
index FZ (x :: _) = x
index (FS k) (_ :: xs) = index k xs

map : (a -> b) -> Maybe a -> Maybe b
map f Nothing = Nothing
map f (Just x) = Just (f x)

tryIndex : { n : Nat } -> Integer -> Vect n a -> Maybe a
tryIndex x xs =
    integerToFin x (S n)
    |> Vect.map (\f => index f xs)

{-
tryIndex 2 (0 :: 1 :: 2 :: Val 3)
Vect.map (flip index (0 :: 1 :: 2 :: Val 3)) (integerToFin 0 4)
-}
