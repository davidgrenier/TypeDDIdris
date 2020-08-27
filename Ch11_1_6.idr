module Ch11_1_6

import Data.Stream
import Data.Primitives.Views

quiz : Stream Int -> Nat -> IO ()
quiz (n1 :: n2 :: nums) score = do
    putStrLn ("Score so far: " ++ show score)
    putStr (show n1 ++ " * " ++ show n2 ++ "? ")
    answer <- getLine
    if cast answer == n1 * n2
        then do
            putStrLn "Correct!"
            quiz nums (score + 1)
        else do
            putStrLn ("Wrong, the answer is " ++ show (n1 * n2))
            quiz nums score

randoms : Int -> Stream Int
randoms seed =
    let seed' = 1664525 * seed + 1013904223 in
    shiftR seed' 2 :: randoms seed'

arithInputs : Int -> Stream Int
arithInputs seed =
    map bound (randoms seed)
    where
        bound : Int -> Int
        bound num with (divides num 12)
            bound (12 * div + rem) | (DivBy div rem prf) = rem + 1
