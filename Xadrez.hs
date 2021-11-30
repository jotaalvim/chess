module Xadrez where 
import Types
import Data.Char

-- rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 0

fenLinha :: String -> [VPeca]
fenLinha "" = []
fenLinha (h:t)
    | elem h "12345678" = replicate (digitToInt h) (Vazio,Neutro) ++ fenLinha t
    | otherwise = auxfenTab h : fenLinha t


fenTab :: String -> Tab
--fenTab "" = []
fenTab s  = map fenLinha linhas
    where linhas = parte '/' s

-- fen to vPeça
auxfenTab :: Char -> VPeca
auxfenTab 'P' = (Peao  ,Branco) 
auxfenTab 'N' = (Cavalo,Branco) 
auxfenTab 'B' = (Bispo ,Branco) 
auxfenTab 'Q' = (Dama  ,Branco) 
auxfenTab 'K' = (Rei   ,Branco) 
auxfenTab 'R' = (Torre ,Branco) 
auxfenTab 'p' = (Peao  ,Preto) 
auxfenTab 'n' = (Cavalo,Preto) 
auxfenTab 'b' = (Bispo ,Preto) 
auxfenTab 'q' = (Dama  ,Preto) 
auxfenTab 'k' = (Rei   ,Preto) 
auxfenTab 'r' = (Torre ,Preto) 
auxfenTab  _  = (Vazio, Neutro)

-- converte fen para Estado
fenEstado :: String -> Estado
fenEstado fen = (t,c, s, enpa, r50, n )
    where [t2,c2,s,enpa,r502,n2] = words fen
          t = fenTab t2
          c = charToCor c2
          r50 = read r502 ::Int
          n   = read n2   ::Int

-- converte o tabuleiro para string
tabToString :: Tab -> [String]
tabToString t = map linhaToString t

-- converte umma lista de vpeças para string
-- show linha
linhaToString :: Linha -> String
linhaToString [] = []
linhaToString ((Peao  ,Branco):t) = ' ':'♙':linhaToString t 
linhaToString ((Cavalo,Branco):t) = ' ':'♘':linhaToString t 
linhaToString ((Bispo ,Branco):t) = ' ':'♗':linhaToString t 
linhaToString ((Dama  ,Branco):t) = ' ':'♕':linhaToString t 
linhaToString ((Rei   ,Branco):t) = ' ':'♔':linhaToString t 
linhaToString ((Torre ,Branco):t) = ' ':'♖':linhaToString t 
linhaToString ((Peao  ,Preto) :t) = ' ':'i':linhaToString t 
--linhaToString ((Peao  ,Preto) :t) = ' ':'♟︎':linhaToString t 
linhaToString ((Cavalo,Preto) :t) = ' ':'♞':linhaToString t 
linhaToString ((Bispo ,Preto) :t) = ' ':'♝':linhaToString t 
linhaToString ((Dama  ,Preto) :t) = ' ':'♛':linhaToString t 
linhaToString ((Rei   ,Preto) :t) = ' ':'♚':linhaToString t 
linhaToString ((Torre ,Preto) :t) = ' ':'♜':linhaToString t 
linhaToString ((Vazio ,Neutro) :t) = ' ':' ':linhaToString t 

--imprime um tab
printTab :: Tab -> IO() 
printTab tab = putStrLn $ unlines $ tabToString tab

--imprime um estado
printEstado :: Estado -> IO() 
printEstado (t,c,_,_,_,n) = 
    do 
        putStrLn $ unlines $ (tabToString t) ++ [show c ++" a jogar"]--,"jogada nº: "++show n]
        putStrLn $ show t

-- converte string para Cor
charToCor :: String -> Cor
charToCor "b" = Preto
charToCor "w" = Branco
charToCor  _  = Neutro

-- parte uma stirng dado um delimitador char
parte :: Char -> String -> [String]
parte c s = foldr f [[]] s
    where f a (h:t)
              | a == c = []:(h:t)
              | otherwise = ((a:h):t)


--printEstado $ fenEstado "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 0"
