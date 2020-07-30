module Utils.Prelude

infixl 5 |>
export
%inline
(|>) : a -> (a -> b) -> b
(|>) = flip ($)

infixl 5 >>
export
%inline
(>>) : (a -> b) -> (b -> c) -> (a -> c)
(>>) f g = g . f
