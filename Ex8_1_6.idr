module Ex8_1_6

same_cons : { xs : List a } -> xs = ys -> x::xs = x::ys
same_cons prf = cong (x ::) prf

same_cons' : { xs : List _ } -> xs = ys -> x::xs = x::ys
same_cons' Refl = Refl

same_lists : { xs : List _ } -> x = y -> xs = ys -> x::xs = y::ys
same_lists Refl Refl = Refl

data ThreeEq : a -> b -> c -> Type where
    Refl : ThreeEq m m m

allSameS : (x,y,z : _) -> ThreeEq x y z -> ThreeEq (S x) (S y) (S z)
allSameS x x x Refl = Refl
