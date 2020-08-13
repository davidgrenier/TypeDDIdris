module Ch8_2

import Utils.Prelude
import Data.Vect
import Data.Nat

-- comm : (left : Nat) -> (right : _) -> left + right = right + left
-- comm 0 0 = Refl
-- comm 0 (S n) = cong S (comm 0 n)
-- comm (S k) 0 = cong S (comm k 0)
-- comm (S k) (S j) = cong S ?rhsComm

reverseProof : Vect (k+1) elem -> Vect (S k) elem
reverseProof { k } xs = rewrite plusCommutative 1 k in xs

myReverse : Vect n elem -> Vect n elem
myReverse [] = []
myReverse (x :: xs) =
    reverseProof (xs ++ [x])

append : Vect n ty -> Vect m ty -> Vect (m+n) ty
append [] ys = rewrite plusZeroRightNeutral m in ys
append { n = S len } (x :: xs) ys =
    rewrite plusCommutative m (S len) in
    x :: rewrite plusCommutative len m in append xs ys

reverse' : Vect l ty -> Vect l ty
reverse' =
    rev []
    where
        proof : Vect (S (n+k)) ty -> Vect (n+S k) ty
        proof { n } { k } (x :: xs) =
            rewrite plusCommutative n (S k) in
            x :: rewrite plusCommutative k n in xs

        rev : Vect n ty -> Vect m ty -> Vect (n+m) ty
        rev { n } xs [] = rewrite plusCommutative n 0 in xs
        rev xs (y :: ys) = proof (rev (y :: xs) ys)
