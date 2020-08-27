module Ex11_1_6

import Data.Stream
import Ch11_1
import Data.List
import Utils.Prelude

everyOther : Stream a -> Stream a
everyOther (x :: y :: xs) = y :: everyOther xs

{-
take 10 (everyOther [1..])
-}

Functor InfList where
    map func (x::xs) = func x :: map func xs

{-
getPrefix 10 (map (*2) (Ch11_1.countFrom 1))
take 10 (countFrom 0)
-}

data Face
    = Heads
    | Tails

coinFlips : Stream Int -> Stream Face
coinFlips (x :: xs) =
    case x `mod` 2 of
    0 => Heads :: coinFlips xs
    _ => Tails :: coinFlips xs

randoms : Int -> Stream Int
randoms seed =
    let seed' = 29 * seed + 101 in
    shiftR seed' 2 :: randoms seed'

mapFace : Face -> Int
mapFace Heads = 0
mapFace Tails = 1

average : Stream Int -> Stream Double
average (x :: xs) =
    average 1 (cast x) xs
    where
        average : Nat -> Double -> Stream Int -> Stream Double
        average count sum (y :: z) =
            sum / cast count :: average (S count) (sum + cast y) z

nth : Nat -> Stream a -> a
nth 0 (x :: y) = x
nth (S k) (x :: y) = nth k y

{-
nth 1000 (average (map mapFace (coinFlips (randoms 12344))))
-}

sqRoot : Double -> Stream Double
sqRoot x =
    root x (x/2)
    where
        root : Double -> Double -> Stream Double
        root target approx =
            let approx' = (approx + target/approx)/2 in
            approx' :: root target approx'

skipWhile : (a -> Bool) -> Stream a -> Stream a
skipWhile f s@(x :: xs) =
    if f x
    then skipWhile f xs
    else s

sqBound : Nat -> Double -> Double -> Double
sqBound max bound x =
    sqBound max (sqRoot x)
    where
        sqBound : Nat -> Stream Double -> Double
        sqBound 0 (approx :: _) = approx
        sqBound (S k) (approx :: rest) =
            if abs(approx*approx-x) > bound
                then sqBound k rest
                else approx

squareRoot : Double -> Double
squareRoot = sqBound 100 0.00000000001
