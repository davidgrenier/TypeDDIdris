module Sins

import Utils.Prelude
import Data.List
import Data.Strings
import System.File

data Format
    = Number Format
    | Str Format
    | Lit String Format
    | Chr Format
    | Dbl Format
    | End

SprintfType : Format -> Type
SprintfType (Number rest) = Int -> SprintfType rest
SprintfType (Str rest) = String -> SprintfType rest
SprintfType (Chr rest) = Char -> SprintfType rest
SprintfType (Dbl rest) = Double -> SprintfType rest
SprintfType (Lit text rest) = SprintfType rest
SprintfType End = String

createFormat : String -> Format
createFormat format =
    create (unpack format)
    where
        create : List Char -> Format
        create [] = End
        create ('%' :: 'd' :: rest) = Number (create rest)
        create ('%' :: 's' :: rest) = Str (create rest)
        create ('%' :: 'c' :: rest) = Chr (create rest)
        create ('%' :: 'f' :: rest) = Dbl (create rest)
        create (c :: rest) =
            case create rest of
            Lit text format => Lit (strCons c text) format
            format => Lit (cast c) format

sprintf : (format : String) -> SprintfType (createFormat format)
sprintf format =
    fmt "" _
    where
        fmt : String -> (format : Format) -> SprintfType format
        fmt acc (Number rest) = \d => fmt (acc ++ cast d) rest
        fmt acc (Str rest) = \s => fmt (acc ++ s) rest
        fmt acc (Chr rest) = \c => fmt (acc ++ cast c) rest
        fmt acc (Dbl rest) = \f => fmt (acc ++ cast f) rest
        fmt acc (Lit text rest) = fmt (acc ++ text) rest
        fmt acc End = acc

TupleVect : (n : Nat) -> Type -> Type
TupleVect 0 a = ()
TupleVect (S k) a = (a, TupleVect k a)

myT : TupleVect 3 Nat
myT = (1, 2, 3, ())
