module Ex8_3_3

import Data.Vect
import Decidable.Equality

headUnequal : DecEq a => { xs : Vect n a } -> { ys : _ } ->
    (contra : x = y -> Void) -> (x :: xs = y :: ys) -> Void
headUnequal contra Refl = contra Refl

tailUnequal : DecEq a => { xs : Vect n a } -> { ys  : _ } ->
    (contra : xs = ys -> Void) -> (x :: xs = y :: ys) -> Void
tailUnequal contra Refl = contra Refl

DecEq a => DecEq (Vect n a) where
    decEq [] [] = Yes Refl
    decEq (x :: xs) (y :: ys) =
        case decEq x y of
        (Yes Refl) =>
            case decEq xs ys of
            (Yes Refl) => Yes Refl
            (No contra) => No (tailUnequal contra)
        (No contra) => No (headUnequal contra)

plusZeroEq : (i : Nat) -> i = i+0
plusZeroEq 0 = Refl
plusZeroEq (S k) = cong S (plusZeroEq k)

plusSucc : (i : Nat) -> (j : _) -> S i + j = i + S j
plusSucc 0 j = Refl
plusSucc (S k) j = cong S (plusSucc k j)

plusComm : (i : Nat) -> (j : Nat) -> i+j = j+i
plusComm 0 0 = Refl
plusComm 0 j = plusZeroEq j
plusComm j 0 = sym (plusZeroEq j)
plusComm k (S j) =
    rewrite sym (plusSucc k j) in cong S (plusComm k j)

rev : { i, j : _ } -> Vect i a -> Vect j a -> Vect (i+j) a
rev { i } { j = 0 } xs [] = rewrite sym (plusZeroEq i) in xs
rev { i } { j = S len } xs (x :: ys) =
    rewrite sym (plusSucc i len) in rev (x :: xs) ys
