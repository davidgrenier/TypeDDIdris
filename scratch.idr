module scratch

import Data.Vect

infixl 5 <|
%inline
(<|) : (a -> b) -> a -> b
(<|) f x = f x
