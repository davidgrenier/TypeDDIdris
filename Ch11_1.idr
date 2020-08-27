module Ch11_1

-- import Data.Stream

-- label : List a -> List (Integer, a)
-- label xs =
--     label 0 xs
--     where
--         label : Integer -> List a -> List (Integer, a)
--         label x [] = []
--         label x (y :: xs) = (x, y) :: label (x+1) xs

public export
data InfList : Type -> Type where
    (::) : (value : a) -> Inf (InfList a) -> InfList a

%name InfList xs, ys, zs

export
countFrom : Nat -> InfList Nat
countFrom x = x :: Delay (Ch11_1.countFrom (S x))

label : List a -> List (Nat, a)
label =
    lbl (countFrom 0)
    where
        lbl : InfList Nat -> List a -> List (Nat, a)
        lbl xs [] = []
        lbl (num :: nums) (x :: xs) = (num, x) :: lbl nums xs

export
getPrefix : Nat -> InfList a -> List a
getPrefix 0 xs = []
getPrefix (S k) (value :: xs) = value :: getPrefix k xs
