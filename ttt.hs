data Valor   = X | Y deriving (Eq,Show)

type Vteor   = Int
type Vreal   = Int

type Vcasilla = Maybe Valor

type Posic = (Int,Int)

type TTT = [Posic] 

data Casilla = Casilla {valor::Vcasilla,
                        vt::   Vteor,
                        vr::   Vreal,
                        ttt::  [TTT] } deriving (Eq,Show)

data Fila    = Fila Casilla Casilla Casilla deriving (Eq,Show)

data Tablero = Tablero Fila Fila Fila deriving (Eq,Show)

fila:: Int -> Tablero -> Fila 
fila n (Tablero f1 f2 f3)
   | n == 1 = f1
   | n == 2 = f2
   | n == 3 = f3

putrow:: Valor -> Int -> Fila -> Fila 
putrow x c (Fila a1 a2 a3)
   | c == 1 = Fila (Casilla (Just x) (vt a1) (vr a1) (ttt a1)) a2 a3
   | c == 2 = Fila a1 (Casilla (Just x) (vt a2) (vr a2) (ttt a2)) a3
   | c == 3 = Fila a1 a2 (Casilla (Just x) (vt a3) (vr a3) (ttt a3))

putval:: Valor -> Posic -> Tablero -> Tablero
putval x (f,c) (Tablero f1 f2 f3)
   | f == 1 = Tablero (putrow x c f1) f2 f3
   | f == 2 = Tablero f1 (putrow x c f2) f3
   | f == 3 = Tablero f1 f2 (putrow x c f3)


putrowreal:: Int -> Int -> Fila -> Fila
putrowreal x c (Fila a1 a2 a3)
   | c == 1 = Fila (Casilla (valor a1) (vt a1) x (ttt a1)) a2 a3
   | c == 2 = Fila a1 (Casilla (valor a2) (vt a2) x (ttt a2)) a3
   | c == 3 = Fila a1 a2 (Casilla (valor a3) (vt a3) x (ttt a3))

putreal::Int -> Posic -> Tablero -> Tablero
putreal x (f,c) (Tablero f1 f2 f3)
   | f == 1 = Tablero (putrowreal x c f1) f2 f3
   | f == 2 = Tablero f1 (putrowreal x c f2) f3
   | f == 3 = Tablero f1 f2 (putrowreal x c f3)

proy:: Casilla -> (Casilla -> a) -> a
proy c f = f c

getvalgeneric:: Int -> Fila -> (Casilla -> a) -> a
getvalgeneric c (Fila a1 a2 a3) f
   | c == 1 = f a1
   | c == 2 = f a2
   | c == 3 = f a3

getrowgeneric:: Posic -> Tablero -> (Casilla -> a) -> a
getrowgeneric (r,c) (Tablero r1 r2 r3) f
   | r == 1 = getvalgeneric c r1 f
   | r == 2 = getvalgeneric c r2 f
   | r == 3 = getvalgeneric c r2 f

getvalrow:: Int -> Fila -> Vcasilla
getvalrow c f = getvalgeneric c f valor

getval:: Posic -> Tablero -> Vcasilla
getval p t = getrowgeneric p t valor

getvalteor:: Posic -> Tablero -> Vteor
getvalteor p t = getrowgeneric p t vt

getvalreal:: Posic -> Tablero -> Vreal
getvalreal p t = getrowgeneric p t vr

playy:: Posic -> Tablero -> Tablero
playy p t = putval Y p (reevaluar t)

playx:: Posic -> Tablero -> Tablero
playx p t = putval X p (reevaluar t)

allpos::[Posic]
allpos = [(x,y) | x <- [1..3], y <- [1..3]]

evaluar::[Posic] -> Tablero -> Tablero
evaluar [] t = t
evaluar (x:xs) t = evaluar xs (putreal (evaluarpos x t) x t)

reevaluar::Tablero -> Tablero
reevaluar t = evaluar allpos t

evaluarpos::Posic -> Tablero -> Vreal
evaluarpos p t = 1 + getvalreal p t


tttvacio  = []

casvacia = Casilla Nothing 0 0 tttvacio

filavacia = Fila casvacia casvacia casvacia 

fila1 = Fila (Casilla Nothing 3 3 [[(1,1),(1,2),(1,3)],[(1,1),(2,2),(3,3)],[(1,1),(2,1),(3,1)]]) 
             (Casilla Nothing 2 2 [[(1,2),(2,2),(3,2)],[(1,1),(1,2),(1,3)]]) 
             (Casilla Nothing 3 3 [[(1,3),(2,3),(3,3)],[(1,3),(2,3),(3,3)],[(1,3),(2,2),(3,1)]])

fila2 = Fila (Casilla Nothing 2 2 [[(2,1),(2,2),(2,3)],[(1,1),(2,2),(3,3)]])
             (Casilla Nothing 4 4 [[(1,1),(2,2),(3,3)],[(2,1),(2,2),(2,3)],[(1,2),(2,2),(3,2)],[(3,1),(2,2),(1,3)]])
             (Casilla Nothing 2 2 [[(1,3),(2,3),(3,3)],[(2,1),(2,2),(2,3)]])

fila3 = Fila (Casilla Nothing 3 3 [[(3,1),(3,2),(3,3)],[(1,1),(2,2),(3,3)],[(3,1),(2,2),(1,3)]]) 
             (Casilla Nothing 2 2 [[(3,2),(2,2),(1,2)],[(3,1),(3,2),(3,3)]]) 
             (Casilla Nothing 3 3 [[(1,3),(2,3),(3,3)],[(3,1),(3,2),(3,3)],[(1,1),(2,2),(3,3)]])

ttest = Tablero fila1 fila2 fila3
     

