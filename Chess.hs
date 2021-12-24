module Chess where 
import Types
import Data.Char

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

-- fen to vPeça
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

-- converte o tabuleiro para string
tabToString :: Board -> [String]
tabToString t = map linhaToString t

-- converte umma lista de vpeças para string
-- show linha
linhaToString :: Line -> String
linhaToString [] = []
linhaToString ((Pawn  ,White):t) = ' ':'♙':linhaToString t 
linhaToString ((Knight,White):t) = ' ':'♘':linhaToString t 
linhaToString ((Bishop ,White):t) = ' ':'♗':linhaToString t 
linhaToString ((Queen  ,White):t) = ' ':'♕':linhaToString t 
linhaToString ((King   ,White):t) = ' ':'♔':linhaToString t 
linhaToString ((Rook ,White):t) = ' ':'♖':linhaToString t 
linhaToString ((Pawn  ,Black) :t) = ' ':'i':linhaToString t 
--linhaToString ((Pawn  ,Black) :t) = ' ':'♟︎':linhaToString t 
linhaToString ((Knight,Black) :t) = ' ':'♞':linhaToString t 
linhaToString ((Bishop ,Black) :t) = ' ':'♝':linhaToString t 
linhaToString ((Queen  ,Black) :t) = ' ':'♛':linhaToString t 
linhaToString ((King   ,Black) :t) = ' ':'♚':linhaToString t 
linhaToString ((Rook ,Black) :t) = ' ':'♜':linhaToString t 
linhaToString ((Empty ,Neutral) :t) = ' ':' ':linhaToString t 

--imprime um tab
printBoard :: Board -> IO() 
printBoard tab = putStrLn $ unlines $ tabToString tab

--imprime um estado
printEstado :: Estado -> IO() 
printEstado (t,c,_,_,_,n) = 
    do 
        putStrLn $ unlines $ (tabToString t) ++ [show c ++" a jogar"]--,"jogada nº: "++show n]
        putStrLn $ show t

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
