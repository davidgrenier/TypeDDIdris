module EqNat

import Utils.Prelude

data EqNat : Nat -> Nat -> Type where
    Same : (num : Nat) -> EqNat num num

data Vect : Nat -> Type -> Type where
    Nil : Vect Z ty
    (::) : ty -> Vect n ty -> Vect (S n) ty

{-
the (EqNat 3 3) (Same _)
Same (2+2)
the (EqNat 3 5) (Same _)
-}

sameS : EqNat k j -> EqNat (S k) (S j)
sameS (Same _) = Same _

checkEqNat : (n : Nat) -> (m : Nat) -> Maybe (EqNat n m)
checkEqNat 0 0 = Just (Same 0)
checkEqNat (S k) (S j) =
    checkEqNat k j
    |> map sameS
checkEqNat _ _ = Nothing

exactLength : { n : _ } -> (len : Nat) -> Vect n a -> Maybe (Vect len a)
exactLength len xs =
    checkEqNat len n
    |> map (\(Same _) => xs)

checkEqNat' : (n : Nat) -> (m : Nat) -> Maybe (n = m)
checkEqNat' 0 0 = Just Refl
checkEqNat' (S k) (S j) =
    checkEqNat' k j
    |> map (cong S)
checkEqNat' _ _ = Nothing
