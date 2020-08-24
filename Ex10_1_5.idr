module Ex10_1_5

import Data.List

data TakeN : List a -> Type where
    Fewer : TakeN xs
    Exact : (xs : _) -> { rest : _ } -> TakeN (xs ++ rest)

total
takeN : (n : Nat) -> (xs : _) -> TakeN xs
takeN 0 xs = Exact []
takeN (S k) [] = Fewer
takeN (S k) (x :: xs) =
    case takeN k xs of
    Fewer => Fewer
    Exact ys => Exact (x :: ys)

groupByN : (n : Nat) -> (xs : List a) -> List (List a)
groupByN n xs with(takeN n xs)
    groupByN n xs | Fewer = [xs]
    groupByN n (ys ++ rest) | Exact ys =
        ys :: groupByN n rest

halves : List a -> (List a, List a)
halves xs with(takeN (length xs `div` 2) xs)
    halves xs | Fewer = ([],[])
    halves (ys ++ rest) | (Exact ys) = (ys, rest)
