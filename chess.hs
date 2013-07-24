module Main where

import Data.List.Split
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
              where coords    = [(x,y) | y <- [0..8] , x <- [0..8]] 
                    pieces    = map readSquare $ filter (/= '\n') initialBoard

writeBoard   :: Board -> String 
writeBoard b = format . map writeSquare $ map snd b 
               where format [] = []
                     format xs = (fst s) ++ "\n" ++ (format $ snd s)
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

-- Game logic functions

finished    :: Board -> Bool
finished b  = False 

move                :: Player ->  (Int, Int) -> (Int, Int) -> Board -> Board
move p from to b    = if isValid source to
                      then b
                      else b
                          where piece  = fromJust $ lookup from b
                                source = (from, piece)
                  
isValid          :: BoardEntry -> (Int, Int) -> Bool
isValid x (y, z) = True

getMoveCoords :: IO ((Int, Int), (Int, Int))
getMoveCoords = do x <- getLine
                   if (length x) == 4
                   then return . fmt $ map digitToInt x
                   else getMoveCoords
                       where fmt [x1, y1, x2, y2] = ((x1,y1), (x2, y2))

gameLoop b  = do putStr "\ESC[2]"
                 putStrLn $ writeBoard b
                 putStrLn "Enter your move in the form: 'xyxy'"
                 coords <- getMoveCoords
                 let b' = move White (fst coords) (snd coords) b
                 if finished b'
                     then putStrLn "Finished!"
                     else gameLoop b'
                 
                 --if finished b
                 --then return ()
                 --else sequence_ [move White (fst coords) (snd coords) b
                 --               ,gameLoop b
                 --               ]

                 --then White
                 --else gameLoop b''
                 --    where  b'  = move White b
                 --           b'' = move Black b'

main = let board = readBoard initialBoard
       in gameLoop board

--putStrLn . writeBoard $ readBoard initialBoard 
