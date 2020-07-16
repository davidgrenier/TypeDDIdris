module Dependents

import Data.Vect

data PowerSource = Petrol | Pedal

data Vehicle : PowerSource -> Type where
    Bicycle : Vehicle Pedal
    Car : (fuel : Nat) -> Vehicle Petrol
    Bus : (fuel : Nat) -> Vehicle Petrol

remaining : Vehicle _ -> Nat
remaining Bicycle = 0
remaining (Car fuel) = fuel
remaining (Bus fuel) = fuel

wheels : Vehicle _ -> Nat
wheels Bicycle = 2
wheels _ = 4

refuel : Vehicle Petrol -> Vehicle Petrol
refuel Bicycle impossible
refuel (Car _) = Car 100
refuel (Bus _) = Bus 200
