module Chess.Validator (isValid) where

import Chess.Types

isValid (from, Just (c, Pawn)) to board = isValidPawn c from to board
isValid _ _ _ = False

checkBounds (x, y) | x >= 0 && x < 8 && y >= 0 && y < 8 = True
checkBounds _ = False

getForward Black (x, y) = (x, y + 1)
getForward White (x, y) = (x, y - 1)
getBackward c l         = getForward (nextColor c) l
getLeft  (x, y)         = (x - 1, y)
getRight (x, y)         = (x + 1, y)
getAdjacent l           = [getLeft l, getForward Black l, getRight l, getBackward Black l]
getForwardDiagonals c l@(x, y) = [left, right]
                                 where (fx, fy) = getForward c l
                                       left     = (fx - 1, fy)
                                       right    = (fx + 1, fy)
getBackwardDiagonals c l = getForwardDiagonals (nextColor c) l

-- | Pawn move
--   If dest is in bounds and
--   If dest is forward, then there is no piece or
--   If dest is diagonal, then there is an enemy piece
isValidPawn c from to b = False
