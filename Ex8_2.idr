module Ex8_2

import Utils.Prelude
import Data.Nat

data Vec : Nat -> ty -> Type where
    Nil : Vec 0 _
    (::) : ty -> Vec n ty -> Vec (S n) ty

eqNat : (a : Nat) -> (b : Nat) -> Maybe (a = b)
eqNat 0 0 = Just Refl
eqNat (S k) (S j) = do
    Refl <- eqNat k j
    Just Refl
eqNat _ _ = Nothing

exactLength : { n : Nat } -> (len : Nat) -> Vec n ty -> Maybe (Vec len ty)
exactLength { n } len xs = do
    Refl <- eqNat n len
    Just xs

eqSucc : a = b -> S a = S b
eqSucc Refl = Refl

plusZeroRight : { k : Nat } -> k = k + 0
plusZeroRight { k = 0 } = Refl
plusZeroRight { k = S k } = eqSucc plusZeroRight

plusSuccRight : (j,k : Nat) -> S j + k = j + S k
plusSuccRight 0 0 = Refl
plusSuccRight 0 (S k) = Refl
plusSuccRight (S j) b = eqSucc (plusSuccRight j b)

eqTrans : a = b -> b = c -> a = c
eqTrans Refl Refl = Refl

plusComm : (a, b : Nat) -> a + b = b + a
plusComm 0 _ = plusZeroRight
plusComm a 0 = sym plusZeroRight
plusComm (S j) (S k) =
    let kj = plusComm j (S k)
        ksj = plusSuccRight k j in
    eqSucc (eqTrans kj ksj)

plusFlip : (a,b,c,d : Nat) -> a + b = c + d -> b + a = d + c
plusFlip a b c d prf =
    let p1 = plusComm b a
        p2 = eqTrans p1 prf
        p3 = plusComm c d in
    eqTrans p2 p3

myReverse : Vec n a -> Vec n a
myReverse =
    rev []
    where
        rev : Vec j b -> Vec k b -> Vec (j+k) b
        rev { j } acc [] =
            rewrite plusComm j 0 in acc
        rev { j } { k = S l } acc (x::xs) =
            rewrite sym (plusSuccRight j l) in rev (x :: acc) xs
