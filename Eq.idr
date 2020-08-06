module Eq

import Utils.Prelude
import Data.List
import Album
import Title

occurences : Eq ty => ty -> List ty -> Nat
occurences item [] = 0
occurences item (value :: values) =
    case value == item of
    True => 1 + occurences item values
    False => occurences item values

data Matter = Solid | Liquid | Gas

Eq Matter where
    (==) Solid Solid = True
    (==) Liquid Liquid = True
    (==) Gas Gas = True
    (==) _ _ = False

    (/=) x y = not (x == y)

data Tree ty
    = Empty
    | Node (Tree ty) ty (Tree ty)

Eq ty => Eq (Tree ty) where
    (==) Empty Empty = True
    (==) (Node x z w) (Node y v s) = z == v && x == y && w == s
    (==) _ _ = False

{-interface Eq ty => Rord ty where
    compare : ty -> ty -> Ordering
    (<) : ty -> ty -> Bool
    (>) : ty -> ty -> Bool
    (<=) : ty -> ty -> Bool
    (>=) : ty -> ty -> Bool

    (<=) x y = x < y || x == y
    (>) x y = not (x <= y)
    (>=) x y = not (x < y)
    (<) x y =
        case compare x y of
        LT => True
        _ => False -}

Ord Matter where
    compare Solid Solid = EQ
    compare Liquid Liquid = EQ
    compare Gas Gas = EQ
    compare Solid Liquid = LT
    compare Liquid Gas = LT
    compare _ _ = GT
