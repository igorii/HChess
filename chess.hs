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

tempBoard :: Board
tempBoard = [((0,1), (White, Pawn))
            ,((1,1), (Black, Pawn))
            ]

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

readSquare  :: Char -> BoardPiece
readSquare c = 
    case c of 
        'r' -> (Black, Rook)
        'n' -> (Black, Knight)
        'b' -> (Black, Bishop)
        'k' -> (Black, King)
        'q' -> (Black, Queen)
        'p' -> (Black, Pawn)
        'R' -> (White, Rook)
        'N' -> (White, Knight)
        'B' -> (White, Bishop)
        'K' -> (White, King)
        'Q' -> (White, Queen)
        'P' -> (White, Pawn)

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
