module Fen

import Base
import Notation

public export
decodeFen : String -> GameState

data FenDecodeError : Type where
    MkFenDecodeError : List String -> FenDecodeError

decodePieceColored : List Char -> Either FenDecodeError PieceColored
decodePieceColored c = case c of
    ['r'] => Right (MkPieceColored { piece = Rook, color = Black })
    ['n'] => Right (MkPieceColored { piece = Knight, color = Black })
    ['b'] => Right (MkPieceColored { piece = Bishop, color = Black })
    ['q'] => Right (MkPieceColored { piece = Queen, color = Black })
    ['k'] => Right (MkPieceColored { piece = King, color = Black })
    ['R'] => Right (MkPieceColored { piece = Rook, color = White })
    ['N'] => Right (MkPieceColored { piece = Knight, color = White })
    ['B'] => Right (MkPieceColored { piece = Bishop, color = White })
    ['Q'] => Right (MkPieceColored { piece = Queen, color = White })
    ['K'] => Right (MkPieceColored { piece = King, color = White })
    _ => Left (MkFenDecodeError ["Invalid FEN"])

decodeActiveColor : List Char -> Either FenDecodeError Color
decodeActiveColor c = case c of
    ['w'] => Right White
    ['b'] => Right Black
    _ => Left (MkFenDecodeError ["Invalid FEN"])

decodeCastlingAvailability : List Char -> Either FenDecodeError (List (Pair Color Side))
decodeCastlingAvailability Nil = Right Nil
decodeCastlingAvailability (c::cs) = case decodeCastlingAvailabilityChar c of
    Right pair => case decodeCastlingAvailability cs of
        Right pairs => Right (pair::pairs)
        Left error => Left (MkFenDecodeError ["Invalid FEN"])
    Left error => Left (MkFenDecodeError ["Invalid FEN"])
    where
        decodeCastlingAvailabilityChar : Char -> Either FenDecodeError (Pair Color Side)
        decodeCastlingAvailabilityChar c = case c of
            'K' => Right (White, KingSide)
            'k' => Right (Black, KingSide)
            'Q' => Right (White, QueenSide)
            'q' => Right (Black, QueenSide)
            _ => Left (MkFenDecodeError ["Invalid FEN"])

decodeEnPassantTargetSquare : List Char -> Either FenDecodeError (Maybe Square)
decodeEnPassantTargetSquare ['-'] = Right Nothing
decodeEnPassantTargetSquare ls = case notationAsSquare ls of
    Right sq => Right (Just sq)
    Left (MkNotationDecodeError strings) => Left (MkFenDecodeError strings)

decodeHalfmoveClock : List Char -> Either FenDecodeError Nat
