module Ex9_1

import Data.List
import Decidable.Equality

data Member : a -> List a -> Type where
    H : Member x (x :: xs)
    T : Member x xs -> Member x (y :: xs)

Uninhabited (Member x []) where
    uninhabited x impossible

data Last : List a -> a -> Type where
    LastOne : Last [value] value
    LastCons : Last xs value -> Last (x :: xs) value

Uninhabited (Last [] x) where
    uninhabited _ impossible

last123 : Last [1,2,3] 3
last123 = LastCons (LastCons LastOne)

isLast : DecEq a => (value : a) -> (xs : List a) -> Dec (Last xs value)
isLast value [] = No absurd
isLast value (x :: xs) =
    case isLast value xs of
    Yes prf => Yes (LastCons prf)
    No contra =>
        case xs of
        [] =>
            case decEq value x of
            Yes Refl => Yes LastOne
            No notEq => No (\(LastOne) => notEq Refl)
        _ => No (\(LastCons proof) => contra proof)
