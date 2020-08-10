module ParameterizedInterfaces

import Utils.Prelude
import Tree

data MyValue ty = Val ty

-- interface Functor f where
--     map : (a -> b) -> f a -> f b

Functor Tree where
    map func Empty = Empty
    map func (Node x y z) = Node (map func x) (func y) (map func z)

Eq ty => Eq (Tree ty) where
    (==) Empty Empty = True
    (==) (Node x z w) (Node x' z' w') = z == z' && x == x' && w == w'
    (==) _ _ = False
