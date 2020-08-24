module Ch10_2

import Data.List

data SnocList : List a -> Type where
    Empty : SnocList []
    Snoc : (x, xs : _) -> (rec : SnocList xs) -> SnocList (xs ++ [x])

appendNilNeutral : (xs : List a) -> xs ++ [] = xs
appendNilNeutral [] = Refl
appendNilNeutral (x :: xs) = cong (x ::) (appendNilNeutral xs)

appendAssoc : (l,c,r : List a) -> l ++ (c ++ r) = (l ++ c) ++ r
appendAssoc [] c r = Refl
appendAssoc (x :: xs) c r = cong (x ::) (appendAssoc xs c r)

total
snocList : (xs : List a) -> SnocList xs
snocList =
    snocList Empty
    where
        snocList : { input : _ } -> (snoc : SnocList input) -> (rest : List a) -> SnocList (input ++ rest)
        snocList {input} snoc [] = rewrite appendNilNeutral input in snoc
        snocList {input} snoc (x :: xs) =
            let result = snocList (Snoc x input snoc) xs in
            rewrite appendAssoc input [x] xs in result

rev : List a -> List a
rev xs =
    rev xs (snocList xs)
    where
        rev : (xs : List a) -> SnocList xs -> List a
        rev [] Empty = []
        rev (ys ++ [x]) (Snoc x ys rec) = x :: rev ys rec

reverse : List a -> List a
reverse xs with(snocList xs)
    reverse [] | Empty = []
    reverse (ys ++ [x]) | (Snoc x ys rec) = x :: Ch10_2.reverse ys | rec

isSuffix : Eq a => List a -> List a -> Bool
isSuffix xs ys with(snocList xs, snocList ys)
    isSuffix [] ys | (Empty, patys) = True
    isSuffix (zs ++ [x]) [] | (Snoc x zs rec, Empty) = False
    isSuffix (zs ++ [x]) (xs ++ [y]) | (Snoc x zs rec, Snoc y xs z) =
        x == y && isSuffix zs xs | (rec, z)
