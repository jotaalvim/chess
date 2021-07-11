import Types

-- devolve a lista de jogadas possíveis para uma peça 
jogadas :: VPeca -> (Int,Int) -> Estado -> [Jogada]
jogadas (Peao,Branco) (x,y) (tab,cor, s1,s2 , r50, n)
    | y == 6 
    | 
