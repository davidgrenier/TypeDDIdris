module Ch11_2

import Data.Stream

data InfIO : Type where
    Do : IO a -> (a -> Inf InfIO) -> InfIO

(>>=) : IO a -> (a -> Inf InfIO) -> InfIO
(>>=) io f = Do io f

loopPrint : String -> InfIO
loopPrint msg = do
    putStrLn msg
    loopPrint msg

run : InfIO -> IO ()
run (Do x f) = do
    result <- x
    run (f result)

data Fuel = Dry | More (Lazy Fuel)

tank : Nat -> Fuel
tank 0 = Dry
tank (S k) = More (tank k)

runOn : Fuel -> InfIO -> IO ()
runOn Dry y = putStrLn "Out of fuel."
runOn (More x) (Do y cont) = do
    result <- y
    runOn x (cont result)

confirm : Stream (IO ())
confirm = ignore getLine :: confirm

runTill : InfIO -> Stream (IO ()) -> IO ()
runTill (Do x cont) (y :: z) = do
    () <- y
    result <- x
    runTill (cont result) z

forever : Fuel
forever = More forever

runForever : InfIO -> IO ()
runForever = runOn forever
