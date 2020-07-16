module Implicits

import Data.Vect

appendEx : (a : Type) -> (n, m : Nat) -> Vect n a -> Vect m a -> Vect (n+m) a
appendEx a 0 m [] ys = ys
appendEx a (S n) _ (x :: xs) ys = x :: appendEx a n _ xs ys

appendIm : { a : Type } -> { n,m : Nat } -> Vect n a -> Vect m a -> Vect (n+m) a
appendIm [] ys = ys
appendIm (x :: xs) ys = x :: appendIm xs ys

append : Vect n a -> Vect m a -> Vect (n+m) a
append [] ys = ys
append (x :: xs) ys = x :: append xs ys

trailWith : Vect n a -> a -> Vect (S n) a
trailWith [] y = [y]
trailWith (x :: xs) y = x :: trailWith xs y

rev : Vect n a -> Vect n a
rev [] = []
rev { n = S k } (x :: xs) = trailWith (rev { n = k } xs) x

len : { n : Nat } -> Vect n a -> Nat
len _ = n

-- empties : { n : _ } -> Vect n (Vect 0 a)
-- empties = replicate _ []

empties : { n : _ } -> Vect n (Vect 0 a)
empties { n = 0 } = []
empties { n = S k } = [] :: empties

{-
Vect.head $ the (Vect 10000000000000000000000000000000000000000000000000 _) empties
-}
