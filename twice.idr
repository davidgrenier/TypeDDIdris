twice : (ty -> ty) -> ty -> ty
twice f = f . f

double : Num ty => ty -> ty
double x = x + x

Shape : Type

rotate : Shape -> Shape

quadruple : Integer -> Integer
quadruple =
    quad
    where
        quad : Integer -> Integer
        quad = twice double
