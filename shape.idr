module shape

Shape : Type
rotate : Shape -> Shape

twice : (a -> a) -> a -> a
twice f = f . f

double : Num a => a -> a
double x = x + x 

quadruple : Num a => a -> a
quadruple = twice double

turnAround : Shape -> Shape
turnAround = twice rotate

test : Int -> String
test x =
    let v = x + x in
    let y = 2 * x in
    show (v*y)

longer : String -> String -> String
longer word1 word2 =
    let w1l = length word1
        w2l = length word2 in
    if w1l > w2l then word1 else word2
