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



printTab :: Tab -> IO() 
printTab tab = putStrL
