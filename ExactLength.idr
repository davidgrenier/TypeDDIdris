module ExactLength

data Vect : Nat -> Type -> Type where
    Nil : Vect Z ty
    (::) : ty -> Vect n ty -> Vect (S n) ty

exactLength : { n : Nat } -> (len : Nat) -> Vect n a -> Maybe (Vect len a)
exactLength { n } len xs =
    if n /= len
        then Nothing
        else Just ?rhs

-- exactLength' : (len : Nat) -> Vect n a -> Maybe (Vect len a)
-- exactLength' 0 [] = Just []
-- exactLength' 0 _ = Nothing
-- exactLength' (S k) [] = Nothing
-- exactLength' (S k) (x :: xs) = do
--     rest <- exactLength' k xs
--     Just (x :: rest)
