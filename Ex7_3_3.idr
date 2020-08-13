module Ex7_3_3

import PreludeInterfaces

Functor Expr where
    map f (Val x) = Val (f x)
    map f (Add x y) = Add (map f x) (map f y)
    map f (Mul x y) = Mul (map f x) (map f y)
    map f (Sub x y) = Sub (map f x) (map f y)
    map f (Div x y) = Div (map f x) (map f y)
    map f (Abs x) = Abs (map f x)

data Vect : Nat -> Type -> Type where
    Nil : Vect Z ty
    (::) : ty -> Vect n ty -> Vect (S n) ty

Eq ty => Eq (Vect n ty) where
    (==) Nil [] = True
    (==) (x :: y) (z :: w) = x == z && y == w

Foldable (Vect n) where
    foldr f acc Nil = acc
    foldr f acc (x :: xs) = f x (foldr f acc xs)
    foldl f acc [] = acc
    foldl f acc (x :: xs) = foldl f (f acc x) xs

{-
foldr (+) 0 (the (Vect _ _) [1,2,3,4,5])
the (Vect _ _) [1,2,3,4] == [1,2,3,4]
the (Vect _ _) [1,2,3,4] == [4,5,6,7]
-}
