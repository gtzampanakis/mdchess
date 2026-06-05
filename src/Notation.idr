module Notation

import Base
import Util

public export
data NotationDecodeError : Type where
    MkNotationDecodeError : List String -> NotationDecodeError

-- Piece

pieceAsNotation : Piece -> List Char
pieceAsNotation p = case p of
    King => ['K']
    Queen => ['Q']
    Bishop => ['B']
    Knight => ['N']
    Rook => ['R']
    Pawn => []

notationAsPiece : List Char -> Either NotationDecodeError Piece
notationAsPiece ls = case ls of
    ['K'] => Right King
    ['Q'] => Right Queen
    ['B'] => Right Bishop
    ['N'] => Right Knight
    ['R'] => Right Rook
    [] => Right Pawn
    _ => Left (MkNotationDecodeError ["Notation Decode Error"])

-- File/Rank

fileAsNotation : File -> List Char
fileAsNotation f = case f of
    (MkFile 0) => ['a']
    (MkFile 1) => ['b']
    (MkFile 2) => ['c']
    (MkFile 3) => ['d']
    (MkFile 4) => ['e']
    (MkFile 5) => ['f']
    (MkFile 6) => ['g']
    (MkFile 7) => ['h']

rankAsNotation : Rank -> List Char
rankAsNotation r = case r of
    (MkRank 0) => ['1']
    (MkRank 1) => ['2']
    (MkRank 2) => ['3']
    (MkRank 3) => ['4']
    (MkRank 4) => ['5']
    (MkRank 5) => ['6']
    (MkRank 6) => ['7']
    (MkRank 7) => ['8']

notationAsFile : List Char -> Either NotationDecodeError File
notationAsFile s = case s of
    ['a'] => Right (MkFile 0)
    ['b'] => Right (MkFile 1)
    ['c'] => Right (MkFile 2)
    ['d'] => Right (MkFile 3)
    ['e'] => Right (MkFile 4)
    ['f'] => Right (MkFile 5)
    ['g'] => Right (MkFile 6)
    ['h'] => Right (MkFile 7)
    _ => Left (MkNotationDecodeError ["NotationDecodeError"])

notationAsRank : List Char -> Either NotationDecodeError Rank
notationAsRank s = case s of
    ['1'] => Right (MkRank 0)
    ['2'] => Right (MkRank 1)
    ['3'] => Right (MkRank 2)
    ['4'] => Right (MkRank 3)
    ['5'] => Right (MkRank 4)
    ['6'] => Right (MkRank 5)
    ['7'] => Right (MkRank 6)
    ['8'] => Right (MkRank 7)
    _ => Left (MkNotationDecodeError ["NotationDecodeError"])

-- Square

squareAsNotation : Square -> List Char
squareAsNotation (MkSquare {file = f, rank = r}) = fileAsNotation f ++ rankAsNotation r

public export
notationAsSquare : List Char -> Either NotationDecodeError Square
notationAsSquare ls = case notationAsFile filePart of
    Left (MkNotationDecodeError strings) => Left (MkNotationDecodeError strings)
    Right file => case notationAsRank rankPart of
        Left (MkNotationDecodeError strings) => Left (MkNotationDecodeError strings)
        Right rank => Right (MkSquare {file = file, rank = rank})
    where
        filePart = filter isAlpha ls
        rankPart = filter isDigit ls
