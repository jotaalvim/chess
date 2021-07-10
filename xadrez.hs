import Data.Char

data Peca = Vazio | Torre | Bispo | Cavalo | Peao | Rei | Dama deriving (Eq,Show)
data Cor  = Preto | Branco | Neutro deriving (Eq,Show)-- neutro para o vazio?

type VPeca = (Peca,Cor)
type Linha = [VPeca]
type Tab = [Linha]

fenLinha :: String -> [VPeca]
fenLinha "" = []
fenLinha (h:t)
    | elem h "12345678" = replicate (digitToInt h) (Vazio,Neutro) ++ fenLinha t
    | otherwise = auxfenTab h : fenLinha t

fenTab :: String -> Tab
--fenTab "" = []
fenTab s  = map fenLinha linhas
    where linhas = parte '/' s

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


parte :: Char -> String -> [String]
parte c s = foldr f [[]] s
    where f a (h:t)
              | a == c = []:(h:t)
              | otherwise = ((a:h):t)

tabToString :: Tab -> [String]
tabToString t = map linhaToString t

linhaToString :: Linha -> String
linhaToString [] = []
linhaToString ((Peao  ,Branco):t) = '♙':linhaToString t 
linhaToString ((Cavalo,Branco):t) = '♘':linhaToString t 
linhaToString ((Bispo ,Branco):t) = '♗':linhaToString t 
linhaToString ((Dama  ,Branco):t) = '♕':linhaToString t 
linhaToString ((Rei   ,Branco):t) = '♔':linhaToString t 
linhaToString ((Torre ,Branco):t) = '♖':linhaToString t 
linhaToString ((Peao  ,Preto) :t) = 'i':linhaToString t 
linhaToString ((Cavalo,Preto) :t) = '♞':linhaToString t 
linhaToString ((Bispo ,Preto) :t) = '♝':linhaToString t 
linhaToString ((Dama  ,Preto) :t) = '♛':linhaToString t 
linhaToString ((Rei   ,Preto) :t) = '♚':linhaToString t 
linhaToString ((Torre ,Preto) :t) = '♜':linhaToString t 
linhaToString ((Vazio, Neutro):t) = ' ':linhaToString t 

printTab :: Tab -> IO() 
printTab tab = putStrLn $ unlines $ tabToString tab
