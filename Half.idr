module Half

import Data.Nat

data Half : Nat -> Type where
    HOdd : (n : Nat) -> Half (S (n+n))
    HEven : (n : Nat) -> Half (n+n)

half : (n : Nat) -> Half n
half 0 = HEven 0
half (S 0) = HOdd 0
half (S (S k)) with(half k)
    half (S (S (S (n + n)))) | (HOdd n) = rewrite plusSuccRightSucc n n in HOdd (S n)
    half (S (S (plus n n))) | (HEven n) = rewrite plusSuccRightSucc n n in HEven (S n)
