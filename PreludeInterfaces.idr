module PreludeInterfaces

import Utils.Prelude

-- interface Show a where
--     show : a -> String

-- interface Num a where
--     (*) : a -> a -> a
--     (+) : a -> a -> a
--     fromInteger : Int -> a

-- interface Num ty => Neg ty where
--     negate : ty -> ty
--     (-) : ty -> ty -> ty
--     abs : ty -> ty

-- interface Num ty => Integral ty where
--     div : ty -> ty -> ty
--     mod : ty -> ty -> ty

-- interface Num ty => Fractional ty where
--     (/) : ty -> ty -> ty
--     recip : ty -> ty
--     recip x = 1/x

record Album where
    constructor MkAlbum
    title : String
    artist : String
    year : Int

Show Album where
    show (MkAlbum t a y) = t ++ ", " ++ a ++ " - " ++ show y

public export
data Expr : ty -> Type where
    Val : ty -> Expr ty
    Add : Expr ty -> Expr ty -> Expr ty
    Mul : Expr ty -> Expr ty -> Expr ty
    Sub : Expr ty -> Expr ty -> Expr ty
    Div : Expr ty -> Expr ty -> Expr ty
    Abs : Expr ty -> Expr ty

export
eval : (Abs ty, Integral ty, Neg ty) => Expr ty -> ty
eval (Val x) = x
eval (Add x y) = eval x + eval y
eval (Sub x y) = eval x - eval y
eval (Mul x y) = eval x * eval y
eval (Div x y) = eval x `div` eval y
eval (Abs x) = abs (eval x)

export
Num ty => Num (Expr ty) where
    (+) = Add
    (*) = Mul
    fromInteger = Val . fromInteger

export
Neg ty => Neg (Expr ty) where
    negate x = 0 - x
    (-) = Sub

export
Abs ty => Abs (Expr ty) where
    abs = Abs

-- the (Expr Int) (6 + 3 * 12) |> eval

Cast (Maybe elem) (List elem) where
    cast (Just x) = [x]
    cast Nothing = []

-- the (List Char) (cast (Just '3'))
