module Types where

data Peca = Vazio | Torre | Bispo | Cavalo | Peao | Rei | Dama deriving (Eq,Show)
data Cor  = Preto | Branco | Neutro deriving (Eq,Show)-- neutro para o vazio?

type VPeca = (Peca,Cor) -- peça final
type Linha = [VPeca]
type Tab = [Linha]
--                   origem    chegada
type Jogada = (Peca,(Int,Int),(Int,Int))
--                       roque  enpa   50  nºjogada
type Estado = (Tab,Cor,String,String,Int,Int) -- quem é a jogar
