module Util

import Data.Vect

public export
joinBy : Vect n String -> String -> String
joinBy Nil _ = ""
joinBy [x] by = x
joinBy (x::xs) by = x ++ by ++ joinBy xs by

data NatDecodeError : Type where
    MkNatDecodeError : List String -> NatDecodeError

charsAsNat : List Char -> Either NatDecodeError Nat
charsAsNat Nil = Left (MkNatDecodeError ["Empty input"])
charsAsNat ls = let
    digitAsNat : Char -> Either NatDecodeError Nat
    digitAsNat c = case c of
        '0' => Right 0
        '1' => Right 1
        '2' => Right 2
        '3' => Right 3
        '4' => Right 4
        '5' => Right 5
        '6' => Right 6
        '7' => Right 7
        '8' => Right 8
        '9' => Right 9
        _ => Left (MkNatDecodeError ["Non-digit"])
    loop : List Char -> Either NatDecodeError (Nat, Nat)
    loop [] = Right (0, 1)
    loop (c::cs) = case digitAsNat c of
        Right n => case loop cs of
            Right (acc, mult) => Right (n * mult + acc, mult * 10)
            Left str => Left str
        Left str => Left str
    in
        case loop ls of
            Right (acc, mult) => Right acc
            Left str => Left str
