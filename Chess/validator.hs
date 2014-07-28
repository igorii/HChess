module Chess.Validator where

import Chess.Types

isValid (from, Just (sourceColor, Pawn)) to board = False
isValid source@(from, piece) to board = True

checkBounds (x, y) | x >= 0 && x < 8 && y >= 0 && y < 8 = True
checkBounds _ = False

getForward Black (x, y) = (x, y + 1)
getForward Black (x, y) = (x, y - 1)


