module scratch

import Data.Vect
import Utils.Prelude

test : () -> Maybe Int
test () = do
    x <- Just 3
    pure x
