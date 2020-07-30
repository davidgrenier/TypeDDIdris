module Utils.Maybe

export
orElse : a -> Maybe a -> a
orElse x Nothing = x
orElse x (Just y) = y
