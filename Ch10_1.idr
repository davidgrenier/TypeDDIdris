module Ch10_1

mcat : List a -> List a -> List a
mcat [] tail = tail
mcat (x :: xs) tail = x :: mcat xs tail

data ListLast : List a -> Type where
    Empty : ListLast []
    NonEmpty : (xs : _) -> (x : _) -> ListLast (mcat xs [x])

listLast : (xs : List a) -> ListLast xs
listLast [] = Empty
listLast (x :: xs) =
    case listLast xs of
    Empty => NonEmpty [] x
    NonEmpty ys y => NonEmpty (x :: ys) y

describeListEnd : List Int -> String
describeListEnd xs with (listLast xs)
    describeListEnd [] | Empty = "Empyt"
    describeListEnd (mcat ys [x]) | (NonEmpty ys x) =
        "Non-empty, initial portion = " ++ show ys

myRev : List a -> List a
myRev xs with (listLast xs)
    myRev [] | Empty = []
    myRev (mcat ys [x]) | (NonEmpty ys x) = x :: myRev ys
