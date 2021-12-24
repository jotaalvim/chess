module Moves where
import Types

--data Peca = Vazio | Rook  | Queen | Knight | Pawn | King | Queen deriving (Eq,Show)
--data Cor  = Black | White | Vazio deriving (Eq,Show)-- neutro para o vazio?

--type VPiece = (Peca,Cor) -- peça final
--type Linha = [VPiece]
--type Board   = [Linha]

--type Jogada = (Peca,Coord,Coord) --enpassent?
--                        roque   enpass
--type Estado = (Board,Cor,String,String,Int,Int) -- quem é a jogar


-- devolve a lista de jogadas possíveis para uma peça

jogadas :: VPiece -> Coord -> Estado -> [Jogada]
jogadas (Pawn,White) (x,y) (tab,cor, s1,s2 , r50, n)
    | y == 6 = 
    | otherwise = Jogada
    where
        a1 = (Pawn,(x,y),(x,y+1))
        a2 = (Pawn,(x,y),(x,y+2))
        cd = (Pawn,(x,y),(x-1,y+1))
        ce = (Pawn,(x,y),(x+1,y+1))

{-
[
[(Rook,Black),(Knight,Black),(Queen,Black),(Queen,Black),(King,Black),(Queen,Black),(Knight,Black),(Rook,Black),
[(Pawn,Black),(Pawn,Black),(Pawn,Black),(Pawn,Black),(Pawn,Black),(Pawn,Black),(Pawn,Black),(Pawn,Black)],
[(Vazio,Neutral),(Vazio,Neutral),(Vazio,Neutral),(Vazio,Neutral),(Vazio,Neutral),(Vazio,Neutral),(Vazio,Neutral),(Vazio,Neutral)],
[(Vazio,Neutral),(Vazio,Neutral),(Vazio,Neutral),(Vazio,Neutral),(Vazio,Neutral),(Vazio,Neutral),(Vazio,Neutral),(Vazio,Neutral)],
[(Vazio,Neutral),(Vazio,Neutral),(Vazio,Neutral),(Vazio,Neutral),(Vazio,Neutral),(Vazio,Neutral),(Vazio,Neutral),(Vazio,Neutral)],
[(Vazio,Neutral),(Vazio,Neutral),(Vazio,Neutral),(Vazio,Neutral),(Vazio,Neutral),(Vazio,Neutral),(Vazio,Neutral),(Vazio,Neutral)],
[(Pawn,White),(Pawn,White),(Pawn,White),(Pawn,White),(Pawn,White),(Pawn,White),(Pawn,White),(Pawn,White)],
[(Rook,White),(Knight,White),(Queen,White),(Queen,White),(King,White),(Queen,White),(Knight,White),(Rook,White)]]
-}

