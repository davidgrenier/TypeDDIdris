module DataTypes

data Direction
    = North
    | South
    | West
    | East

clockwise : Direction -> Direction
clockwise North = East
clockwise South = West
clockwise West = North
clockwise East = South

repeat : Nat -> (a -> a) -> a -> a
repeat 0 f = id
repeat (S k) f = f . repeat k f

{-
     map (repeat 4 clockwise) [North,East,West,South]
-}

data Shape : Type where
    Triangle : Double -> Double -> Shape
    Rectangle :  Double -> Double -> Shape
    Circle : Double -> Shape

area : Shape -> Double
area (Triangle base height) = base*height/2
area (Rectangle length height) = length*height
area (Circle radius) = pi*radius*radius


{-
    Triagle 2.0 3.0
    Circle 2.0
    Rectangle 2.0 4.0
-}

data Natural
    = Zero
    | Succ Natural

data Picture
    = Primitive Shape
    | Composition Picture Picture
    | Rotation Double Picture
    | Translation Double Double Picture

%name Shape shape, shape1, shape2
%name Picture pic, pic1, pic2

testPicture : Picture
testPicture =
    let (+) = Composition
        t = \w,h => Translation w h . Primitive in
    t 5 5 (Rectangle 20 10)
    + t 35 5 (Circle 5)
    + t 15 25 (Triangle 10 10)

pictureArea : Picture -> Double
pictureArea (Primitive p) = area p
pictureArea (Composition p1 p2) = pictureArea p1 + pictureArea p2
pictureArea (Translation _ _ p) = pictureArea p
pictureArea (Rotation _ p) = pictureArea p

data Biggest = NoTriangle | Size Double

biggestTriangle : Picture -> Biggest
biggestTriangle (Primitive shape) = NoTriangle
biggestTriangle (Composition pic1 pic2) =
    case (biggestTriangle pic1, biggestTriangle pic2) of
    (NoTriangle, s2) => s2
    (s1, NoTriangle) => s1
    (Size s1, Size s2) => Size (max s1 s2)
biggestTriangle (Rotation _ pic) = biggestTriangle pic
biggestTriangle (Translation _ _ pic) = biggestTriangle pic
