Haskell Chess
=============

This is my first attempt at writing a useful Haskell program. I am currently reading the wonderful "Haskell School of Expression" Book by Paul Hudak of Yale. I am finding Haskell extremely intriguing, and thought I would take a stab at creating a Chess application (console-based) in Haskell. The application works via the honor system. I've decided to put this project on hold while I learn more so that I can more effectively implement an AI and a move validator.

In any case, here is my first attempt at Haskell...

Since the application is a single file, I've declared this as the Main module.

>module Main where

I've then imported some packages for list operations and the Maybe monad.

>import Data.List.Split
>import Data.List
>import Data.Maybe
>import Data.Char

Here I define the *algebraic data types* I'll be using in the application. `Player` will reflect the two types of players in the game, White and Black.

>data Player     = White 
>                | Black 
>                deriving (Show, Eq)

Piece reflects the various chess pieces that are used. For both of these data types, I've derived Show and Eq. This allows me to compare `Player`s with other `Player`s (White /= Black, etc), and write the types to stdout. 

>data Piece      = Pawn
>                | Rook
>                | Bishop
>                | Knight
>                | King
>                | Queen
>                deriving (Show, Eq)

Here I provide some type declarations that will allow the code to be more concise. The *type* keyword, rather than defining a new data type, simply creates a *synonym* for a data type, or compound type. `BoardPiece` reflects a piece of a certain colour, ie (Black, Knight). `BoardEntry` gives a 2D location to a `BoardPiece`. Lastly, `Board` is simply a list of `BoardEntrie`s.

>type BoardPiece = (Player, Piece) 
>type BoardEntry = ((Int, Int), Maybe BoardPiece) 
>type Board      = [BoardEntry]

This is the initial setup of the chess board. Black pieces are on top, while White pieces are on the bottom.

>initialBoard :: String
>initialBoard = unlines ["rnbkqbnr"
>                       ,"pppppppp"
>                       ,"........"
>                       ,"........"
>                       ,"........"
>                       ,"........"
>                       ,"PPPPPPPP"
>                       ,"RNBQKBNR"
>                       ]

The first order of business in creating the game logic will be reading and writing a board. Reading will take a string representation of the board (as above), and translate it into a `Board` type. The inverse of that operation would then be to write the board. Writing would take a `Board` and return a string representation. 

To read, a list comprehension is created that returns the set of 2D coordinates in the square bound by (0,0) and (7,7). Beside this set, another list, called `pieces` is created which represents the internal piece type for each character in the string. These two lists are joined together using `zip`, producing a single list of the form [(Int, Int), (Player, Piece)], which is the definition of the `Board` type.

>readBoard   :: String -> Board
>readBoard s = zip coords pieces
>              where coords    = [(x,y) | y <- [0..7] , x <- [0..7]] 
>                    pieces    = map readSquare $ filter (/= '\n') initialBoard

As the inverse of `readBoard`, `writeBoard` takes a `Board` type and creates a list of characters, one character for each element in the board. After the string is constructed, newlines (\n) are inserted every 8 characters to create the affect of a 2D board.

>writeBoard   :: Board -> String 
>writeBoard b = "  01234567\n" ++ (format 0 $ map writeSquare $ map snd b) 
>               where format i [] = []
>                     format i xs = (show i) ++ " " ++ (fst s) ++ "\n" ++ (format (i+1) $ snd s)
>                        where s = splitAt 8 xs

I then had to map characters to pieces. There may be a much simpler way to do this with Haskell magic, but I opted for a case statement to be explicit.

>readSquare  :: Char -> Maybe BoardPiece
>readSquare c = 
>   case c of 
>       'r' -> Just (Black, Rook)
>       'n' -> Just (Black, Knight)
>       'b' -> Just (Black, Bishop)
>       'k' -> Just (Black, King)
>       'q' -> Just (Black, Queen)
>       'p' -> Just (Black, Pawn)
>       'R' -> Just (White, Rook)
>       'N' -> Just (White, Knight)
>       'B' -> Just (White, Bishop)
>       'K' -> Just (White, King)
>       'Q' -> Just (White, Queen)
>       'P' -> Just (White, Pawn)
>       otherwise -> Nothing

And the inverse of the above, a mapping of pieces to characters.

>writeSquare  :: Maybe BoardPiece -> Char
>writeSquare p = 
>    case p of 
>        Just (Black, Rook)   -> 'r'  
>        Just (Black, Knight) -> 'n' 
>        Just (Black, Bishop) -> 'b' 
>        Just (Black, King)   -> 'k' 
>        Just (Black, Queen)  -> 'q' 
>        Just (Black, Pawn)   -> 'p' 
>        Just (White, Rook)   -> 'R' 
>        Just (White, Knight) -> 'N' 
>        Just (White, Bishop) -> 'B' 
>        Just (White, King)   -> 'K' 
>        Just (White, Queen)  -> 'Q' 
>        Just (White, Pawn)   -> 'P' 
>        otherwise -> '.'

Finally, the game logic bits. The following functions are largely unfinished, but provide enoigh functionality to have a game of chess on the honor system. `finished` should be a function that returns false when either player is in checkmate. For now, it always returns False, keeping the game in play until manually shut down.

>finished    :: Board -> Bool
>finished b  = False 

Then comes the tricky part (for me). I had to define how to take a piece and move it to another location on the board. The next function handles this logic by taking a player (the player who is making the move), a from location, a to location, and a board, and returns the new board with the move applied. First, the source piece is obtained via a `lookup` into the board. Then the to location is updated to the source piece. Lastly, the from location is cleared by setting Nothing as
the piece.

>move                :: Player ->  (Int, Int) -> (Int, Int) -> Board -> Board
>move p from to b    = if isValid source to b
>                      then b''
>                      else b
>                          where piece          = fromJust $ lookup from b
>                                source         = (from, piece)
>                                b'             = setPiece p (from, piece) to b
>                                b''            = setPiece p (to, Nothing) from b'
                                
>setPiece            :: Player -> BoardEntry -> (Int, Int) -> Board -> Board 
>setPiece p pc loc b = let i = fromJust $ elemIndex (loc, fromJust $ lookup loc b) b
>                          (x,_:ys) = splitAt i b 
>                      in x ++ [(loc, snd pc)] ++ ys
          
The next function would be the move validator, but after taking a stab at this, and producing a large chunk of gross code, I've chosen to wait until I learn more of the language. For now I'll return True for every proposed move.

>isValid       :: BoardEntry -> (Int, Int) -> Board -> Bool
>isValid x y z = True       
                                   
Next, for some IO! This function handles retrieving the source and destination locations from the player. Currently, the expected format is '0033', which translates to source <- (0,0), destination <- (3,3).

>getMoveCoords :: IO ((Int, Int), (Int, Int))
>getMoveCoords = do x <- getLine
>                   if (length x) == 4
>                   then return . fmt $ map digitToInt x
>                   else getMoveCoords
>                       where fmt [x1, y1, x2, y2] = ((x1,y1), (x2, y2))

The gameloop is fairly simple. Write the board, get the move, perform the move, check whether the game is finished and repeat if not.

>gameLoop b  = do putStrLn $ writeBoard b
>                 putStrLn "Enter your move in the form: 'xyxy'"
>                 coords <- getMoveCoords
>                 print coords
>                 let b' = move White (fst coords) (snd coords) b
>                 if finished b'
>                     then putStrLn "Finished!"
>                     else gameLoop b'
                 
Lastly, the driver for the whole application, the main function. Here the initial board is loaded, and the gameloop is started.

>main = let board = readBoard initialBoard
>       in gameLoop board

I'm sure there is a lot done poorly in the above code. I'd love to learn what I could be doing better in future projects, so is there is anything that is striking you as wildly stupid, or just a silly way of doing something, please feel free to leave a suggestion in the comments below (if reading this at timthornton.net/blog).

I'd like to continue this project later when I've learned more about the language. I'm currently fascinated by Haskell, and quite enjoy the book by Hudak. 

If you're still reading at this point, thanks for hanging in there! It means a lot! :)
