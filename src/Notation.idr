module Notation

import Base

total
pieceAsNotation : Piece -> String
pieceAsNotation p = case p of
    King => "K"
    Queen => "Q"
    Bishop => "B"
    Knight => "N"
    Rook => "R"
    Pawn => ""

total
fileAsNotation : File -> String
fileAsNotation f = case f of
    (MkFile 0) => "a"
    (MkFile 1) => "b"
    (MkFile 2) => "c"
    (MkFile 3) => "d"
    (MkFile 4) => "e"
    (MkFile 5) => "f"
    (MkFile 6) => "g"
    (MkFile 7) => "h"

total
rankAsNotation : Rank -> String
rankAsNotation r = case r of
    (MkRank 0) => "1"
    (MkRank 1) => "2"
    (MkRank 2) => "3"
    (MkRank 3) => "4"
    (MkRank 4) => "5"
    (MkRank 5) => "6"
    (MkRank 6) => "7"
    (MkRank 7) => "8"

total
squareAsNotation : Square -> String
squareAsNotation (MkSquare {file = f, rank = r}) = fileAsNotation f ++ rankAsNotation r
