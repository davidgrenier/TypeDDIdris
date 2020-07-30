module Depends

import Utils.Prelude
import Data.Vect

readVectLen : (n : _) -> IO (Vect n String)
readVectLen 0 = pure []
readVectLen (S k) = do
    line <- getLine
    xs <- readVectLen k
    pure $ line :: xs

data VectUnknown : Type -> Type where
    MkVect : (n : _) -> Vect n a -> VectUnknown a

readVect : IO (VectUnknown String)
readVect = do
    "" <- getLine
        | input => do
            MkVect _ xs <- readVect
            pure $ MkVect _ (input :: xs)
    pure (MkVect _ [])

printVect : Show a => VectUnknown a -> IO ()
printVect (MkVect n xs) = putStrLn (show xs ++ "(length " ++ show n ++ ")")

readV2 : IO (n ** Vect n String)
readV2 = do
    "" <- getLine
        | input => do
            (_ ** xs) <- readV2
            pure (_ ** input :: xs)
    pure (Z ** [])

printV2 : Show a => (n ** Vect n a) -> IO ()
printV2 (n ** xs) = putStrLn (show xs ++ " (length " ++ show n ++ ")")

-- exactLength : (n : _) -> Vect m a -> Maybe (Vect n a)
-- exactLength 0 [] = Just []
-- exactLength 0 (x :: xs) = Nothing
-- exactLength (S k) [] = Nothing
-- exactLength (S k) (x :: xs) = do
--     rest <- Depends.exactLength k xs
--     pure (x :: rest)

zipInputs : IO ()
zipInputs = do
    putStrLn "Enter the first vector:"
    (n ** xs) <- readV2
    putStrLn "Enter the second vector:"
    (_ ** ys) <- readV2
    Just ys <- pure $ exactLength n ys
        | Nothing => putStrLn "Vectors are different lengths."
    printLn (zip xs ys)
