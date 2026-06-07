module Util

import Data.Vect

public export
joinBy : Vect n String -> String -> String
joinBy Nil _ = ""
joinBy [x] by = x
joinBy (x::xs) by = x ++ by ++ joinBy xs by

public export
data NatDecodeError : Type where
    MkNatDecodeError : List String -> NatDecodeError

public export
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

public export
isWhitespace : Char -> Bool
isWhitespace c = case c of
    ' ' => True
    '\t' => True
    '\n' => True
    '\r' => True
    _ => False

public export
splitStringByWhitespace : String -> List String
splitStringByWhitespace ls = map pack (splitListCharByWhitespace (unpack ls))
where
    splitListCharByWhitespace : List Char -> List (List Char)
    splitListCharByWhitespace [] = []
    splitListCharByWhitespace (c0::cs) = case isWhitespace c0 of
        True => splitListCharByWhitespace cs
        False => case cs of
            [] => [c0]::[]
            (c1::cs) => case isWhitespace c1 of
                True => [c0]::splitListCharByWhitespace cs
                False => case splitListCharByWhitespace (c1::cs) of
                    [] => [] -- absurd
                    x::xs => (c0::x)::xs

-- public export
-- firstLeftOrListRights : (a -> Either b c) -> List a -> Either b (List c)
-- firstLeftOrListRights _ [] = Right []
-- firstLeftOrListRights fn (x::xs) = case fn x of
--     Left left => Left left
--     Right right => case firstLeftOrListRights fn xs of
--         Left left => Left left
--         Right rights => Right (right::rights)
