module Chess where 

import Types
import Data.Char
import Data.List

-- rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 0

fenLine :: String -> [VPiece]
fenLine "" = []
fenLine (h:t)
    | elem h "12345678" = replicate (digitToInt h) (Empty,Neutral) ++ fenLine t
    | otherwise = auxfenBoard h : fenLine t


fenBoard :: String -> Board
--fenBoard "" = []
fenBoard s  = map fenLine linhas
    where linhas = parte '/' s

-- fen to VPiece
auxfenBoard :: Char -> VPiece
auxfenBoard 'P' = (Pawn  ,White) 
auxfenBoard 'N' = (Knight,White) 
auxfenBoard 'B' = (Bishop ,White) 
auxfenBoard 'Q' = (Queen  ,White) 
auxfenBoard 'K' = (King   ,White) 
auxfenBoard 'R' = (Rook ,White) 
auxfenBoard 'p' = (Pawn  ,Black) 
auxfenBoard 'n' = (Knight,Black) 
auxfenBoard 'b' = (Bishop ,Black) 
auxfenBoard 'q' = (Queen  ,Black) 
auxfenBoard 'k' = (King   ,Black) 
auxfenBoard 'r' = (Rook ,Black) 
auxfenBoard  _  = (Empty, Neutral)

-- converte fen para Estado
fenEstado :: String -> Estado
fenEstado fen = (t,c, s, enpa, r50, n )
    where [t2,c2,s,enpa,r502,n2] = words fen
          t = fenBoard t2
          c = charToColor c2
          r50 = read r502 ::Int
          n   = read n2   ::Int


-- converts a VPiece to a String
-- show linha
vpiece2string :: VPiece -> String
vpiece2string (Pawn  , White)   = "♙"
vpiece2string (Knight, White)   = "♘"
vpiece2string (Bishop, White)   = "♗"
vpiece2string (Queen , White)   = "♕"
vpiece2string (King  , White)   = "♔"
vpiece2string (Rook  , White)   = "♖"
vpiece2string (Pawn  , Black)   = "i"
--vpiece2string (Pawn  ,Black) = "♟︎"
vpiece2string (Knight, Black)   = "♞"
vpiece2string (Bishop, Black)   = "♝"
vpiece2string (Queen , Black)   = "♛"
vpiece2string (King  , Black)   = "♚"
vpiece2string (Rook  , Black)   = "♜"
vpiece2string (Empty , Neutral) = " "

board2string :: Board -> String
board2string b = unlines $ map (intercalate " | ") $ map (map vpiece2string) b

--imprime um tab
printBoard :: Board -> IO() 
printBoard b = putStrLn $ board2string b

--imprime um estado
printEstado :: Estado -> IO() 
printEstado (t,c,_,_,_,n) = 
    do 
        putStrLn $ (board2string t)-- ++ [show c ++" a jogar"]--,"jogada nº: "++show n]
        --putStrLn $ show t

-- converte string para Color
charToColor :: String -> Color
charToColor "b" = Black
charToColor "w" = White
charToColor  _  = Neutral

-- parte uma stirng dado um delimitador char
parte :: Char -> String -> [String]
parte c s = foldr f [[]] s
    where f a (h:t)
              | a == c = []:(h:t)
              | otherwise = ((a:h):t)


--printEstado $ fenEstado "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 0"
