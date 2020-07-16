module Exercices4_2

import Data.Vect

data Power = Petrol | Pedal | Electric

data Vehicle : Power -> Type where
    Bicycle : Vehicle Pedal
    Car : (fuel : Double) -> Vehicle Petrol
    Truck : (fuel : Double) -> Vehicle Petrol
    Unicycle : Vehicle Pedal
    Motorcycle : (fuel : Double) -> Vehicle Petrol
    Tram : (voltage : Nat) -> Vehicle Electric
    ElectricCar : (voltage : Nat) -> Vehicle Electric

wheels : Vehicle power -> Nat
wheels Bicycle = 2
wheels (Motorcycle _) = 2
wheels (Car _) = 4
wheels (Truck _) = 4
wheels Unicycle = 1
wheels (Tram _) = 18
wheels (ElectricCar _) = 4

refuel : Vehicle Petrol -> Vehicle Petrol
refuel (Car _) = Car 100
refuel (Truck _) = Truck 200
refuel (Motorcycle _) = Motorcycle 50

vectTake : (k : Fin (S n)) -> Vect n a -> Vect (finToNat k) a
vectTake FZ _ = []
vectTake (FS x) (y :: xs) = y :: vectTake x xs

{-
vectTake 4 [1,2,3,4]
-}

sumEntries : Num a => { n : Nat } -> Integer -> Vect n a -> Vect n a -> Maybe a
sumEntries _ [] _ = Nothing
sumEntries k (x :: xs) (y :: ys) =
    case integerToFin k n of
    Just FZ => Just (x + y)
    Just (FS k) => sumEntries (cast $ finToNat k) xs ys
    Nothing => Nothing
