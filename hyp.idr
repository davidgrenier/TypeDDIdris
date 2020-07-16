import Data.Strings
import Data.List

hyp : (Double, Double) -> Double
hyp (x,y) =
    let sq = \x => x*x in
    sqrt (sq x + sq y)

lengths : String -> List Nat
lengths = (map length) . words

average : List Nat -> Double
average nums =
    let (c,t) = foldr acc (0,0) nums in
    cast t / cast c

    where
        acc : Nat -> (Nat, Nat) -> (Nat, Nat)
        acc x (c, t) = (c+1,t+x)
