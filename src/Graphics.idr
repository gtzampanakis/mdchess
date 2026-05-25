module Graphics

import Data.Vect

import Base
import Util

pieceColoredAsGraphics : PieceColored -> String
pieceColoredAsGraphics (MkPieceColored {piece = piece, color = color}) = case (piece, color) of
    (King, White) => "K"
    (Queen, White) => "Q"
    (Bishop, White) => "B"
    (Knight, White) => "N"
    (Rook, White) => "R"
    (Pawn, White) => "P"
    (King, Black) => "k"
    (Queen, Black) => "q"
    (Bishop, Black) => "b"
    (Knight, Black) => "n"
    (Rook, Black) => "r"
    (Pawn, Black) => "p"

squareContentAsGraphics : SquareContent -> String
squareContentAsGraphics (SquareFilled pieceColored) = pieceColoredAsGraphics pieceColored
squareContentAsGraphics SquareEmpty = " "

rankContentsAsGraphics : RankContents -> String
rankContentsAsGraphics (MkRankContents vectOfSquareContent) = joinBy strings " | " where
    strings = map squareContentAsGraphics vectOfSquareContent

boardContentsAsGraphics : BoardContents -> String
boardContentsAsGraphics (MkBoardContents vectOfRankContents) =
    joinBy (map rankContentsAsGraphics vectOfRankContents) "\n"
