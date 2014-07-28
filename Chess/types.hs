module Chess.Types where

import Data.List.Split
import Data.List
import Data.Maybe
import Data.Char

-- Data and type defines

data Player     = White 
                | Black 
                deriving (Show, Eq)

data Piece      = Pawn
                | Rook
                | Bishop
                | Knight
                | King
                | Queen
                deriving (Show, Eq)

type BoardPiece = (Player, Piece) 
type BoardEntry = ((Int, Int), Maybe BoardPiece) 
type Board      = [BoardEntry]

-- The initial state of the game, black on top

initialBoard :: String
initialBoard = unlines ["rnbkqbnr"
                       ,"pppppppp"
                       ,"........"
                       ,"........"
                       ,"........"
                       ,"........"
                       ,"PPPPPPPP"
                       ,"RNBQKBNR"
                       ]

-- Read and write functions

readBoard   :: String -> Board
readBoard s = zip coords pieces
              where coords    = [(x,y) | y <- [0..7] , x <- [0..7]] 
                    pieces    = map readSquare $ filter (/= '\n') initialBoard

writeBoard   :: Board -> String 
writeBoard b = "  01234567\n" ++ (format 0 $ map writeSquare $ map snd b) 
               where format i [] = []
                     format i xs = (show i) ++ " " ++ (fst s) ++ "\n" ++ (format (i+1) $ snd s)
                        where s = splitAt 8 xs

readSquare  :: Char -> Maybe BoardPiece
readSquare c = 
    case c of 
        'r' -> Just (Black, Rook)
        'n' -> Just (Black, Knight)
        'b' -> Just (Black, Bishop)
        'k' -> Just (Black, King)
        'q' -> Just (Black, Queen)
        'p' -> Just (Black, Pawn)
        'R' -> Just (White, Rook)
        'N' -> Just (White, Knight)
        'B' -> Just (White, Bishop)
        'K' -> Just (White, King)
        'Q' -> Just (White, Queen)
        'P' -> Just (White, Pawn)
        otherwise -> Nothing

-- Inverse of readSquare
writeSquare  :: Maybe BoardPiece -> Char
writeSquare p = 
    case p of 
        Just (Black, Rook)   -> 'r'  
        Just (Black, Knight) -> 'n' 
        Just (Black, Bishop) -> 'b' 
        Just (Black, King)   -> 'k' 
        Just (Black, Queen)  -> 'q' 
        Just (Black, Pawn)   -> 'p' 
        Just (White, Rook)   -> 'R' 
        Just (White, Knight) -> 'N' 
        Just (White, Bishop) -> 'B' 
        Just (White, King)   -> 'K' 
        Just (White, Queen)  -> 'Q' 
        Just (White, Pawn)   -> 'P' 
        otherwise -> '.'

setPiece            :: Player -> BoardEntry -> (Int, Int) -> Board -> Board 
setPiece p pc loc b = let i = fromJust $ elemIndex (loc, fromJust $ lookup loc b) b
                          (x,_:ys) = splitAt i b 
                      in x ++ [(loc, snd pc)] ++ ys
