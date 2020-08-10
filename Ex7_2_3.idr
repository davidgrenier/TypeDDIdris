module Ex7_2_3

import PreludeInterfaces

Show ty => Show (Expr ty) where
    show (Val x) = show x
    show (Add x y) = "(" ++ show x ++ " + " ++ show y ++ ")"
    show (Mul x y) = show x ++ " * " ++ show y
    show (Sub x y) = "(" ++ show x ++ " - " ++ show y ++ ")"
    show (Div x y) = show x ++ " / " ++ show y
    show (Abs x) = "abs(" ++ show x ++ ")"

-- show (the (Expr Integer) 2+3*4)

(Abs ty, Integral ty, Neg ty, Eq ty) => Eq (Expr ty) where
    (==) x y = eval x == eval y

-- the (Expr Integer) (2+3*4) == Val 14
-- the (Expr Integer) (2+3*4) == Val 16

(Neg ty, Integral ty, Abs ty) => Cast (Expr ty) ty where
    cast = eval

-- the Integer (cast $ the (Expr Integer) 2+3*4)
