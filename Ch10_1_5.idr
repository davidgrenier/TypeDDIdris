module Ch10_1_5

import Data.List

m : Ord a => List a -> List a -> List a
m [] ys = ys
m xs [] = xs
m (x :: xs) (y :: ys) =
    case compare x y of
    LT => x :: m xs (y :: ys)
    _ => y :: m (x :: xs) ys

data SplitList : List a -> Type where
    SplitPair : (left : List a) -> (right : List a) -> SplitList (left ++ right)

total
splitList : (xs : List a) -> SplitList xs
splitList xs =
    splitList xs xs
    where
        splitList : List a -> (xs : List a) -> SplitList xs
        splitList xs [] = SplitPair [] []
        splitList xs [x] = SplitPair [] [x]
        splitList (_ :: _ :: counter) (y :: ys) =
            case splitList counter ys of
            SplitPair [] [] => SplitPair [] [y]
            SplitPair [] [x] => SplitPair [y] [x] 
            SplitPair left right => SplitPair (y :: left) right
        splitList _ ys = SplitPair [] ys

halves : List a -> (List a, List a)
halves xs =
    halves xs xs
    where
        halves : List a -> List a -> (List a, List a)
        halves (_ :: _ :: counter) (x :: xs) =
            let (left, right) = halves counter xs in
            (x :: left, right)
        halves _ ys = ([], ys)

mergeSort : Ord a => List a -> List a
mergeSort xs with (splitList xs)
mergeSort (_ ++ []) | (SplitPair _ []) = []
mergeSort (left ++ [x]) | (SplitPair left [x]) = m left [x]
mergeSort (left ++ right) | (SplitPair left right) =
    m (mergeSort left) (mergeSort right)
