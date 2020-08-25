module Ch11_1

-- import Data.Stream

-- label : List a -> List (Integer, a)
-- label xs =
--     label 0 xs
--     where
--         label : Integer -> List a -> List (Integer, a)
--         label x [] = []
--         label x (y :: xs) = (x, y) :: label (x+1) xs

data InfList : Type -> Type where
    (::) : (value : a) -> Inf (InfList a) -> InfList a

%name InfList xs, ys, zs

countFrom : Nat -> InfList Nat
countFrom x = x :: countFrom (S x)

label : List a -> List (Nat, a)
label =
    lbl (countFrom 0)
    where
        lbl : InfList Nat -> List a -> List (Nat, a)
        lbl xs [] = []
        lbl (num :: nums) (x :: xs) = (num, x) :: lbl nums xs

getPrefix : Nat -> InfList a -> List a
getPrefix 0 xs = []
getPrefix (S k) (value :: xs) = value :: getPrefix k xs
