module Ch8_3

twoPlusTwoNotFive: 2+2=5 -> Void
twoPlusTwoNotFive Refl impossible

valueNotSucc : {x : _} -> x = S x -> Void
valueNotSucc Refl impossible

twoPlusTwoEqFour : Dec (2+2 = 4)
twoPlusTwoEqFour = Yes Refl

twoPlusTwoEqFive : Dec (2+2 = 5)
twoPlusTwoEqFive = No twoPlusTwoNotFive

prevEq : S k = S j -> k = j
prevEq Refl = Refl

zeroNotSucc : 0 = S _ -> Void
zeroNotSucc Refl impossible

succNotZero : S _ = 0 -> Void
succNotZero Refl impossible

noRec : (k = j -> Void) -> S k = S j -> Void
noRec contra Refl = contra Refl

checkEqNat : (num1 : Nat) -> (num2 : _) -> Dec (num1 = num2)
checkEqNat 0 0 = Yes Refl
checkEqNat 0 (S k) = No zeroNotSucc
checkEqNat (S k) 0 = No succNotZero
checkEqNat (S k) (S j) =
    case checkEqNat k j of
    (Yes prf) => Yes (cong S prf)
    -- (No contra) => No (contra . prevEq)
    (No contra) => No (noRec contra)
