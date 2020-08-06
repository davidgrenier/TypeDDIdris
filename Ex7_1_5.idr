module Ex7_1_5

import Data.List

data Shape
    = Triangle Double Double
    | Rectangle Double Double
    | Circle Double

Eq Shape where
    (==) (Triangle x z) (Triangle x' z') = x == x' && z == z'
    (==) (Rectangle x z) (Rectangle x' z') = x == x' && z == z'
    (==) (Circle x) (Circle x') = x == x'
    (==) _ _ = False

{-
Circle 4 == Circle 4
Circle 4 == Circle 5
Circle 4 == Triangle 3 2
-}

area : Shape -> Double
area (Triangle x y) = x*y/2
area (Rectangle x y) = x*y
area (Circle r) = pi*r*r

Ord Shape where
    compare s1 s2 = compare (area s1) (area s2)

result : List Shape
result =
    sort [
        Rectangle 2 6,
        Triangle 3 9,
        Rectangle 2 7,
        Circle 3,
        Circle 4
    ]
