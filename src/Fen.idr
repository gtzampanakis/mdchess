module Fen

import Base
import Notation
import Util

data FenDecodeError : Type where
    MkFenDecodeError : List String -> FenDecodeError

decodePiecePlacement : List Char -> Either FenDecodeError BoardContents
decodePiecePlacement ls = Left (MkFenDecodeError ["Invalid FEN"])

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

decodeCastlingAvailability : List Char -> Either FenDecodeError CastlingAvailability
decodeCastlingAvailability Nil = Right (MkCastlingAvailability Nil)
decodeCastlingAvailability (c::cs) = case decodeCastlingAvailabilityChar c of
    Right pair => case decodeCastlingAvailability cs of
        Right (MkCastlingAvailability pairs) => Right (MkCastlingAvailability (pair::pairs))
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

decodeEnPassantTargetSquare : List Char -> Either FenDecodeError EnPassantTargetSquare
decodeEnPassantTargetSquare ['-'] = Right (MkEnPassantTargetSquare Nothing)
decodeEnPassantTargetSquare ls = case notationAsSquare ls of
    Right sq => Right (MkEnPassantTargetSquare (Just sq))
    Left (MkNotationDecodeError strings) => Left (MkFenDecodeError strings)

decodeHalfmoveClock : List Char -> Either FenDecodeError Nat
decodeHalfmoveClock ls = case charsAsNat ls of
    Right n => Right n
    Left (MkNatDecodeError strings) => Left (MkFenDecodeError strings)

decodeFullmoveNumber : List Char -> Either FenDecodeError Nat
decodeFullmoveNumber ls = case charsAsNat ls of
    Right n => Right n
    Left (MkNatDecodeError strings) => Left (MkFenDecodeError strings)

public export
decodeFen : String -> Either FenDecodeError GameState
decodeFen s = case map unpack (splitStringByWhitespace s) of
    [a, b, c, d, e, f]
        => gameStateIfAllRights (fa a) (fb b) (fc c) (fd d) (fe f) (ff f)
    _ => Left (MkFenDecodeError ["Invalid FEN"])
where
    fa = decodePiecePlacement
    fb = decodeActiveColor
    fc = decodeCastlingAvailability
    fd = decodeEnPassantTargetSquare
    fe = decodeHalfmoveClock
    ff = decodeFullmoveNumber
    gameStateIfAllRights :
                     Either FenDecodeError BoardContents
                  -> Either FenDecodeError Color
                  -> Either FenDecodeError CastlingAvailability
                  -> Either FenDecodeError EnPassantTargetSquare
                  -> Either FenDecodeError Nat
                  -> Either FenDecodeError Nat
                  -> Either FenDecodeError GameState
    gameStateIfAllRights (Right r1) (Right r2) (Right r3) (Right r4) (Right r5) (Right r6)
        = Right (MkGameState r1 r2 r3 r4 r5 r6)
    gameStateIfAllRights _ _ _ _ _ _
        = Left (MkFenDecodeError ["Invalid FEN"])
