module Base

import public Data.Fin
import Data.Vect

public export
NFiles : Nat
NFiles = 8

public export
NRanks : Nat
NRanks = 8

public export
data Piece = King | Queen | Bishop | Knight | Rook | Pawn

public export
data Color = White | Black

data Motion =
    Up | Right | Down | Left |
    DiagUpRight | DiagDownRight | DiagDownLeft | DiagUpLeft | 
    KnightUpRight | KnightRightUp | KnightRightDown | KnightDownRight |
    KnightDownLeft | KnightLeftDown | KnightLeftUp | KnightUpLeft

public export
record PieceColored where
    constructor MkPieceColored
    piece : Piece
    color : Color

public export
data File : Type where
    MkFile : Fin NFiles -> File

public export
data Rank : Type where
    MkRank : Fin NRanks -> Rank

public export
record Square where
    constructor MkSquare
    file : File
    rank : Rank

record Move where
    constructor MkMove
    squareFrom: Square
    squareTo: Square
    promotionTo: Maybe Piece

public export
data SquareContent = SquareFilled PieceColored | SquareEmpty

public export
data RankContents : Type where
    MkRankContents : (Vect NFiles SquareContent) -> RankContents

public export
data BoardContents = MkBoardContents (Vect NRanks RankContents)
