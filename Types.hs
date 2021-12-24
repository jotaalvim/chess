module Types where

data Piece = Empty | Rook  | Bishop | Knight  | Pawn | King | Queen deriving (Eq,Show)
data Color = Black | White | Neutral deriving (Eq,Show)-- neutro para o vazio?

type VPiece = (Piece,Color) -- peça final
type Line   = [VPiece]
type Board  = [Line]
type Coord  = (Int,Int) 
--                   origem    chegada
type Play = (Piece,Coord,Coord) --enpassent?
--                          roque  enpa   50  nºjogada
type Estado = (Board,Color,String,String,Int,Int) -- quem é a jogar
