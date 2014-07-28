module Chess.Validator where

import Chess.Types

isValid (from, Just (sourceColor, Pawn)) to board = False
isValid source@(from, piece) to board = True


