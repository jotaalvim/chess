module Movimento where
import Types

--data Peca = Vazio | Torre  | Bispo | Cavalo | Peao | Rei | Dama deriving (Eq,Show)
--data Cor  = Preto | Branco | Vazio deriving (Eq,Show)-- neutro para o vazio?

--type VPeca = (Peca,Cor) -- peça final
--type Linha = [VPeca]
--type Tab   = [Linha]

--type Jogada = (Peca,(Int,Int),(Int,Int)) --enpassent?
--                        roque   enpass
--type Estado = (Tab,Cor,String,String,Int,Int) -- quem é a jogar


-- devolve a lista de jogadas possíveis para uma peça

jogadas :: VPeca -> (Int,Int) -> Estado -> [Jogada]
jogadas (Peao,Branco) (x,y) (tab,cor, s1,s2 , r50, n)
    | y == 6 = 
    | otherwise = Jogada
    where
        a1 = (Peao,(x,y),(x,y+1))
        a2 = (Peao,(x,y),(x,y+2))
        cd = (Peao,(x,y),(x-1,y+1))
        ce = (Peao,(x,y),(x+1,y+1))

{-
[
[(Torre,Preto),(Cavalo,Preto),(Bispo,Preto),(Dama,Preto),(Rei,Preto),(Bispo,Preto),(Cavalo,Preto),(Torre,Preto),
[(Peao,Preto),(Peao,Preto),(Peao,Preto),(Peao,Preto),(Peao,Preto),(Peao,Preto),(Peao,Preto),(Peao,Preto)],
[(Vazio,Neutro),(Vazio,Neutro),(Vazio,Neutro),(Vazio,Neutro),(Vazio,Neutro),(Vazio,Neutro),(Vazio,Neutro),(Vazio,Neutro)],
[(Vazio,Neutro),(Vazio,Neutro),(Vazio,Neutro),(Vazio,Neutro),(Vazio,Neutro),(Vazio,Neutro),(Vazio,Neutro),(Vazio,Neutro)],
[(Vazio,Neutro),(Vazio,Neutro),(Vazio,Neutro),(Vazio,Neutro),(Vazio,Neutro),(Vazio,Neutro),(Vazio,Neutro),(Vazio,Neutro)],
[(Vazio,Neutro),(Vazio,Neutro),(Vazio,Neutro),(Vazio,Neutro),(Vazio,Neutro),(Vazio,Neutro),(Vazio,Neutro),(Vazio,Neutro)],
[(Peao,Branco),(Peao,Branco),(Peao,Branco),(Peao,Branco),(Peao,Branco),(Peao,Branco),(Peao,Branco),(Peao,Branco)],
[(Torre,Branco),(Cavalo,Branco),(Bispo,Branco),(Dama,Branco),(Rei,Branco),(Bispo,Branco),(Cavalo,Branco),(Torre,Branco)]]
-}

