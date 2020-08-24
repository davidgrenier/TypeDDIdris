module Ex10_2_4

import Data.List
import Data.List.Views
import Data.Vect
import Data.Nat
import Data.Strings

equalSuffix : Eq a => List a -> List a -> List a
equalSuffix xs ys with(snocList xs, snocList ys)
    equalSuffix [] _ | (Empty, _) = []
    equalSuffix _ [] | (_, Empty) = []
    equalSuffix (zs ++ [z]) (ys ++ [y]) | (Snoc z zs sz, Snoc y ys sy) =
        if z == y
            then (equalSuffix zs ys | (sz, sy)) ++ [z]
            else []

data Split : (n : Nat) -> Vect n a -> Type where
    Splitted : (left : Vect i a) -> (right : Vect j a) -> Split (i+j) (left ++ right)

split : (xs : Vect n a) -> Split n xs
split xs =
    split xs xs
    where
        split : (counter : Vect k a) -> (right : Vect m a) -> Split m right
        split counter [] = Splitted [] []
        split (_ :: _ :: counter) (x :: ys) =
            let Splitted left right = split counter ys in
            Splitted (x :: left) right
        split _ ys = Splitted [] ys

myMerge : Ord a => Split k xs -> Vect k a
myMerge (Splitted left right) =
    merge (myMerge (split left)) (myMerge (split right))

mergeSort : Ord a => Vect n a -> Vect n a
mergeSort xs = myMerge (split xs)

-- data HRec : (n : Nat) -> Type where
--     HZero : HRec 0
--     HEven : (n : Nat) -> HRec n -> HRec (n+n)
--     HOdd : (n : Nat) -> HRec n -> HRec (S (n+n))

-- hRec : (n : Nat) -> HRec n
-- hRec 0 = HZero
-- hRec (S k) with (hRec k)
--     hRec (S 0) | HZero = HOdd 0 HZero
--     hRec (S (n+n)) | (HEven n halfn) = HOdd n halfn
--     hRec (S (S (n + n))) | (HOdd n halfn) =
--         rewrite plusSuccRightSucc (S n) n in (HEven (S n) ?rhs)

toBinary : Nat -> String
toBinary n =
    case toBin [] n of
    [] => "0"
    xs => pack xs
    where
        toBin : List Char -> Nat -> List Char
        toBin xs k with (halfRec k)
        toBin xs 0 | HalfRecZ = xs
        toBin xs (plus j j) | (HalfRecEven j rec) = toBin ('0' :: xs) j
        toBin xs (S (j + j)) | (HalfRecOdd j rec) = toBin ('1' :: xs) j

data VList : List a -> Type where
    VEmpty : VList []
    VOne : (x : a) -> VList [x]
    VPair : (left, right : a) -> (xs : List a) -> (rec : VList xs) -> VList (left :: xs ++ [right])

vList : (xs : List a) -> VList xs
vList [] = VEmpty
vList [x] = VOne x
vList (x :: xs) with (snocList xs)
    vList (x :: []) | Empty = VOne x
    vList (x :: (ys ++ [y])) | (Snoc y ys rec) =
        VPair x y ys (vList ys)

palindrome : List Char -> Bool
palindrome xs with (vList xs)
palindrome [] | VEmpty = True
palindrome [x] | (VOne x) = True
palindrome (left :: (ys ++ [right])) | (VPair left right ys rec) =
    left == right && palindrome ys

{-
palindrome (unpack "abccba")
palindrome (unpack "abcba")
palindrome (unpack "abcb")
-}
