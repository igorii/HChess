module Chess where

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
type BoardEntry = ((Int, Int), BoardPiece)
type Board      = [BoardEntry] 

-- A temporary board for testing
tempBoard :: Board
tempBoard = [((0,1), (White, Pawn))
            ,((1,1), (Black, Pawn))
            ]
-- TODO: delete the above when board loading works

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
loadBoard s = undefined

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
main = undefined 


{-
 -  loadBoard
 -  if notFinished
 -  then do
 -      whiteMove b
 -      blackMove b
 -  else
 -      quit
 -}
