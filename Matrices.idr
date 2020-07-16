module Matrices

import Data.Vect

infixl 5 |>
(|>) : a -> (a -> b) -> b
(|>) x f = f x

Matrix : Nat -> Nat -> Type -> Type
Matrix n m a = Vect n (Vect m a)

addV : Num a => Vect n a -> Vect n a -> Vect n a
addV xs ys = zipWith (+) xs ys

add : Num a => Matrix n m a -> Matrix n m a -> Matrix n m a
add [] [] = []
add (x :: xs) (y :: ys) = addV x y :: add xs ys

transpose : { m : _ } -> Matrix n m a -> Matrix m n a
transpose [] = replicate _ []
transpose (x :: xs) =
    Matrices.transpose xs
    |> zipWith (::) x

mult : Num a => { n,p : _ } -> Matrix n m a -> Matrix m p a -> Matrix n p a
mult [] _ = []
mult _ [] = replicate _ (replicate _ 0)
mult xs ys =
    let ysTr = Matrices.transpose ys in
    xs |> map (\x => ysTr |> map (sum . zipWith (*) x))

scalarProd : Num a => { n : _ } -> Vect n a -> a
scalarProd xs =
    let [[res]] =
        Matrices.transpose [xs]
            |> Matrices.mult [xs] in
    res
