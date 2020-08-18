module Ch9

import Data.Nat
import Data.Vect
import Decidable.Equality

data Member : a -> Vect k a -> Type where
    H : Member x (x :: xs)
    T : (later : Member x xs) -> Member x (y :: xs)

oneInVector : Member 1 [2,1,3]
oneInVector = T H

oneInVector2 : Member 1 [3,2,1]
oneInVector2 = T (T H)

maryInVector : Member "Mary" ["Peter", "Paul", "Mary"]
maryInVector = T (T H)

Uninhabited (Member value []) where
    uninhabited prf impossible

removeElem : { n : Nat } -> (value : a) -> (xs : Vect (S n) a ) ->
    {auto proof : Member value xs} -> Vect n a
removeElem value (value :: xs) { proof = H } = xs
removeElem { n = 0 } value (y :: []) { proof = T later } = absurd later
removeElem { n = S k } value (y :: xs) { proof = T later } = y :: removeElem value xs

removeElem' : { n : Nat } -> (value : a) -> (xs : Vect (S n) a) ->
    {auto prf : Elem value xs} -> Vect n a
removeElem' value (value :: xs) { prf = Here } = xs
removeElem' { n = 0} value (y :: []) { prf = There later} = absurd later
removeElem' { n = S _} value (y :: xs) { prf = There _} = y :: removeElem' value xs

combine : (x = y -> Void) -> (Elem x xs -> Void) -> Elem x (y :: xs) -> Void
combine notHere _ Here = notHere Refl
combine _ notThere (There later) = notThere later

isElem : DecEq a => (value : a) -> (xs : Vect n a) -> Dec (Elem value xs)
isElem value [] = No uninhabited
isElem value (x :: xs) =
    case decEq value x of
    Yes Refl => Yes Here
    No contra =>
        case Ch9.isElem value xs of
        Yes prf => Yes (There prf)
        No contra2 => No (combine contra contra2)
