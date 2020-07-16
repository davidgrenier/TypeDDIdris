module Generics

data DivResult = DivByZero | Value Double

safeDiv : Double -> Double -> DivResult
safeDiv x 0 = DivByZero
safeDiv x y = Value (x/y)

data Option a
    = Some a
    | None

safeDiv2 : Double -> Double -> Option Double
safeDiv2 x 0 = None
safeDiv2 x y = Some (x/y)

infixr 5 |-|
data Chain a = Nothing | (|-|) a (Chain a)

{-
1 |-| 2 |-| Nothing
-}

data Result a b = Success a | Failure b

infixl 5 >=
(>=) : Result a b -> (a -> Result c b) -> Result c b
(>=) (Success x) f = f x
(>=) (Failure x) f = Failure x

{-
Failure 3 >= (Failure . (+ 4))
-}

data Tree elem
    = Empty
    | Node (Tree elem) elem (Tree elem)

%name Tree tree, tree1, tree2

depth : Tree a -> Nat
depth Empty = Z
depth (Node left x right) = 1 + max (depth left) (depth right)

insert : Ord a => a -> Tree a -> Tree a
insert x Empty = Node Empty x Empty
insert x tree@(Node low y high) =
    case compare x y of
    LT => Node (insert x low) y high
    EQ => tree
    GT => Node low y (insert x high)
