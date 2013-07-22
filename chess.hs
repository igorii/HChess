module Main where

import Data.List.Split

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

loadBoard   :: String -> Board
loadBoard s = zip coords pieces
              where coords = [(x,y) | y <- [0..8] , x <- [0..8]] 
                    pieces = map readSquare $ filter (/= '\n') initialBoard

writeBoard   :: Board -> String 
writeBoard b = format . map writeSquare $ map snd b 
               where format [] = []
                     format xs = (fst s) ++ "\n" ++ (format $ snd s)
                        where s = splitAt 8 xs


-- Given a character, try to return the player and piece
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

finished    :: Board -> Bool
finished b  = False 

move        :: Player -> Board -> Board
move p b    = undefined

gameLoop    :: Board -> Player
gameLoop b  = if finished b
                then White
                else gameLoop b''
              where  b'  = move White b
                     b'' = move Black b'

main :: IO ()
main = putStrLn . writeBoard $ loadBoard initialBoard 

{-
 -  loadBoard
 -  if notFinished
 -  then do
 -      whiteMove b
 -      blackMove b
 -  else
 -      quit
 -}
