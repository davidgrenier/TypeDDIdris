module Synonyms

import Data.Vect
import Data.Strings
import System.File

Coord : Type
Coord = (Double, Double)

Polygon : Nat -> Type
Polygon k = Vect k Coord

tri : Polygon 3
tri = [(0.0,0.0), (0.0,3.0), (0.0,4.0)]

StringOrInt : Bool -> Type
StringOrInt True = Int
StringOrInt False = String

getStringOrInt : (isInt : Bool) -> StringOrInt isInt
getStringOrInt True = 22
getStringOrInt False = "Twenty two"

StringOrBool : Bool -> Type
StringOrBool True = String
StringOrBool False = Bool

fromStringOrBool : (isString : Bool) -> StringOrBool isString -> String
fromStringOrBool True = id
fromStringOrBool False = show

printStringOrInt : (isInt : Bool) -> StringOrInt isInt -> String
printStringOrInt True x = cast x
printStringOrInt False x = trim x

valToString : (isInt : Bool) -> (case isInt of
                                    True => Int
                                    False => String) -> String
valToString True x = cast x
valToString False x = trim x

Summands : Type -> (n : Nat) -> Type
Summands a 0 = a
Summands a (S k) = a -> Summands a k

mySum : Num a => (n : Nat) -> Summands a n
mySum =
    ssum 0
    where
        ssum : a -> (n : Nat) -> Summands a n
        ssum acc Z = acc
        ssum acc (S k) = \x => ssum (x + acc) k

Tprintf : List Char -> Type
Tprintf [] = String
Tprintf ('%' :: 'd' :: rest) = Int -> Tprintf rest
Tprintf ('%' :: 's' :: rest) = String -> Tprintf rest
Tprintf (c :: rest) = Tprintf rest

fmt : String -> (format : List Char) -> Tprintf format
fmt acc [] = acc
fmt acc ('%' :: 'd' :: rest) = \d => fmt (acc ++ cast d) rest
fmt acc ('%' :: 's' :: rest) = \s => fmt (acc ++ s) rest
fmt acc (c :: rest) = fmt (acc ++ cast c) rest
