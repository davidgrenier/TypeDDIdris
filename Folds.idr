module Folds

import Tree

-- interface Fldable f where
--     foldl : (acc -> b -> acc) -> acc -> f b -> acc

-- Fldable List where
--     foldl f acc [] = acc
--     foldl f acc (x :: xs) = Folds.foldl f (f acc x) xs

Functor Tree where
    map f Empty = Empty
    map f (Node x y z) = Node (map f x) (f y) (map f z)

insert : ty -> Tree ty -> Tree ty
insert x Empty = Node Empty x Empty
insert x (Node y z w) = Node (insert x y) z w

Foldable Tree where
    -- Optional
    -- foldl f acc Empty = acc
    -- foldl f acc (Node x y z) = foldl f (f (foldl f acc x) y) z
    foldr f acc Empty = acc
    foldr f acc (Node x y z) = foldr f (f y (foldr f acc z)) x

merge : Tree ty -> Tree ty -> Tree ty
merge x y = foldr insert y x

toList : Tree a -> List a
toList t = foldr (::) [] t

{-
toList $ merge (Node (Node Empty 1 Empty) 1 (Node (Node (Node Empty 1 Empty) 1 Empty) 2 Empty)) (Node (Node (Node Empty 1 Empty) 1 Empty) 2 Empty)
-}

Applicative Tree where
    (<*>) Empty _ = Empty
    (<*>) _ Empty = Empty
    (<*>) (Node x y z) t = merge (x <*> t) (merge (map y t) (z <*> t))
    pure v = Node Empty v Empty

totalLen : Foldable f => f String -> Nat
totalLen xs = foldl (\acc, x => length x + acc) 0 xs

{-
totalLen ["One", "Two", "Three"]
totalLen (Node (Node Empty "One" Empty) "Two" (Node Empty "Three" Empty))
-}

Monad Tree where
    -- join Empty = Empty
    -- join (Node x y z) = merge (join x) (merge y (join z))
    (>>=) Empty f = Empty
    (>>=) (Node x y z) f =
        merge (merge (x >>= f) (f y)) (z >>= f)

test : Tree Int
test = do
    x <- Node Empty 3 (Node Empty 4 Empty)
    y <- Node (Node Empty 2 Empty) 8 Empty
    Node Empty (x+y) Empty
{-
the (List _) (pure "test")
the (Maybe _) (pure "test")
-}
