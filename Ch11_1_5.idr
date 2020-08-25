module Ch11_1_5

import Data.Stream

myRepeat : a -> Stream a
myRepeat x = x :: myRepeat x

myTake : Nat -> Stream a -> List a
myTake 0 x = []
myTake (S k) (x :: xs) = x :: myTake k xs

myIterate : (a -> a) -> a -> Stream a
myIterate f x = x :: myIterate f (f x)

filtered : (a -> Bool) -> (a -> a) -> a -> Stream a
filtered f g x =
    if f x
        then x :: filtered f g (g x)
        else filtered f g (g x)

primes : Stream Nat
primes =
    2 :: primes (\k => k `mod` 2 /= 0) (S . S) 3
        where
            primes : (Nat -> Bool) -> (Nat -> Nat) -> Nat -> Stream Nat
            primes f g p =
                if f p
                    then p :: primes (\k => f k && k `mod` p /= 0) g (g p)
                    else primes f g (g p)

labelWith : Stream labelType -> List a -> List (labelType, a)
labelWith labels [] = []
labelWith (label :: labels) (x :: xs) = (label, x) :: labelWith labels xs

label : List a -> List (Integer, a)
label = labelWith (iterate (+1) 0)

logistic : Double -> Stream Double
logistic r = iterate (\k => r*k*(1-k)) 0.2

stabilize : Stream Double -> Double
stabilize (x :: y) =
    stable x y
    where
        stable : Double -> Stream Double -> Double
        stable prev (x :: y) =
            if abs(prev - x) < 0.001
                then x
                else stable x y

{-
take 20 (logistic 0.9)
stabilize (logistic 2.6)
map ((/ 10.0) . cast) [1..20]
stabilize (logistic 0.9)
-}
