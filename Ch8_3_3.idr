module Ch8_3_3

import Data.Vect
import Decidable.Equality

exactLength : { n : Nat } -> (len : Nat) -> Vect n ty -> Maybe (Vect len ty)
exactLength { n } len xs =
    case decEq len n of
    Yes Refl => Just xs
    No contra => Nothing
