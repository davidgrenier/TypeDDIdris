import Data.Vect

insertBefore : Vect n a -> (a -> Bool) -> a -> Vect (S n) a
insertBefore [] f x = [x]
insertBefore (y :: xs) f x =
    if f y then x :: y :: xs
    else y :: insertBefore xs f x

insertion : Ord a => Vect n a -> Vect n a
insertion [] = []
insertion (x :: xs) =
    let sorted = insertion xs in
    insertBefore sorted (\y => y > x) x

insert : Ord a => a -> Vect n a -> Vect (S n) a
insert x [] = [x]
insert x (y :: xs) =
    case x < y of
    True => x :: y :: xs
    False => y :: insert x xs

insSort : Ord a => Vect n a -> Vect n a
insSort [] = []
insSort (x :: xs) = insert x $ insSort xs
