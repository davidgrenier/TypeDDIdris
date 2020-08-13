module Constrained

data BSTree : Type -> Type where
    Empty : Ord a => BSTree a
    Node : Ord a => BSTree a -> a -> BSTree a -> BSTree a

insert : a -> BSTree a -> BSTree a
insert x Empty = Node Empty x Empty
insert x tree@(Node low y high) =
    case compare x y of
    LT => Node (insert x low) y high
    EQ => tree
    GT => Node low y (insert x high)

listToTree : Ord a => List a -> BSTree a
listToTree [] = Empty
listToTree (x :: xs) = insert x (listToTree xs)

treeToList : BSTree a -> List a
treeToList Empty = []
treeToList (Node x y z) = treeToList x ++ [y] ++ treeToList z

{-
listToTree [1,4,3,5,2]
treeToList $ listToTree [4,1,8,7,2,3,9,5,6]
-}

public export
data Expr
    = Val Int
    | Add Expr Expr
    | Sub Expr Expr
    | Mult Expr Expr

evaluate : Expr -> Int
evaluate (Val x) = x
evaluate (Add x y) = evaluate x + evaluate y
evaluate (Sub x y) = evaluate x - evaluate y
evaluate (Mult x y) = evaluate x * evaluate y

{-
evaluate (Mult (Val 10) (Add (Val 6) (Val 3)))
-}

maxMaybe : Ord a => Maybe a -> Maybe a -> Maybe a
maxMaybe (Just x) (Just y) = Just (max x y)
maxMaybe Nothing y = y
maxMaybe x Nothing = x

{-
maxMaybe Nothing (Just 3)
-}
