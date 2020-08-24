module Ch10_2_4

import Data.List
import Data.List.Views

mergeSort : Ord a => List a -> List a
mergeSort xs with(splitRec xs)
    mergeSort [] | SplitRecNil = []
    mergeSort [x] | (SplitRecOne x) = [x]
    mergeSort (lefts ++ rights) | (SplitRecPair lefts rights lrec rrec) =
        merge (mergeSort lefts | lrec) (mergeSort rights | rrec)
