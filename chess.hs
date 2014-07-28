module Main where

import Data.List.Split
import Data.List
import Data.Maybe
import Data.Char

import Chess.Types
import Chess.Validator

isValid _ _ _ = True

finished   :: Board -> Bool
finished b = False 

move             :: Player -> (Int, Int) -> (Int, Int) -> Board -> Board
move p from to b = if isValid source to b
                   then b''
                   else b
                       where piece  = fromJust $ lookup from b
                             source = (from, piece)
                             b'     = setPiece p (from, piece) to b
                             b''    = setPiece p (to, Nothing) from b'

getMoveCoords :: IO ((Int, Int), (Int, Int))
getMoveCoords = do x <- getLine
                   if (length x) == 4
                   then return . fmt $ map digitToInt x
                   else getMoveCoords
                       where fmt [x1, y1, x2, y2] = ((x1,y1), (x2, y2))

gameLoop b = do putStrLn $ writeBoard b
                putStrLn "Enter your move in the form: 'xyxy'"
                coords <- getMoveCoords
                print coords
                let b' = move White (fst coords) (snd coords) b
                if finished b'
                    then putStrLn "Finished!"
                    else gameLoop b'

main = let board = readBoard initialBoard
       in gameLoop board
