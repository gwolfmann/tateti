--import Data.Tuple.Select
import Data.Char

data Valor   = X | Y deriving (Eq,Show)

type Vteor   = Int
type Vreal   = Int

type Vcasilla = Maybe Valor

type Posic = (Int,Int)

type TTT = [Posic]

data Casilla = Casilla {valor::Vcasilla,
                        vt::   Vteor,
                        vr::   Vreal,
                        ttt::  [TTT] } deriving (Eq)

data Fila    = Fila Casilla Casilla Casilla deriving (Eq,Show)

data Tablero = Tablero Fila Fila Fila deriving (Eq)

instance Show Casilla where
   show (Casilla v vt vr ttt) = show v

instance Show Tablero where
   show (Tablero f1 f2 f3) = show f1 ++ "\n" ++ show f2 ++ "\n" ++ show f3


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
   | r == 3 = getvalgeneric c r3 f

getvalrow:: Int -> Fila -> Vcasilla
getvalrow c f = getvalgeneric c f valor

getval:: Posic -> Tablero -> Vcasilla
getval p t = getrowgeneric p t valor

getvalteor:: Posic -> Tablero -> Vteor
getvalteor p t = getrowgeneric p t vt

getvalreal:: Posic -> Tablero -> Vreal
getvalreal p t = getrowgeneric p t vr

getvalttt:: Posic -> Tablero -> [TTT]
getvalttt p t = getrowgeneric p t ttt

evalpos:: Tablero -> Posic -> Vreal
evalpos t p = evalttts t p $ getvalttt p t

evalttts:: Tablero -> Posic -> [TTT] -> Vreal
evalttts t p ttts = foldl max 0 $ map (evalttt t p) ttts

evalttt:: Tablero -> Posic -> TTT -> Vreal
evalttt t p ttt 
   | ocupado p t     = 0
   | dosmios  t ttt  = 10 
   | dosrival t ttt  = 8
   | unoyuno  t ttt  = 1
   | unomio   t ttt  = 7
   | unorival t ttt  = 5
   | triovacio t ttt = getvalteor p t
   | otherwise       = 0

ttttoval::Tablero -> TTT -> [Vcasilla]
ttttoval t ttt = map (\x -> getval x t ) ttt

triovacio::Tablero -> TTT -> Bool
triovacio t ttt = all (\x -> x == Nothing) $ ttttoval t ttt

dosmios::Tablero -> TTT -> Bool
dosmios t ttt = 2 == count (Just Y) (ttttoval t ttt)

dosrival::Tablero -> TTT -> Bool
dosrival t ttt = 2 == count (Just X) (ttttoval t ttt)

unomio t ttt   = 1 == count (Just Y) (ttttoval t ttt)

unorival t ttt = 1 == count (Just X) (ttttoval t ttt)

unoyuno t ttt = (1 == count (Just X) (ttttoval t ttt)) && (1 == count (Just Y) (ttttoval t ttt))

ocupado :: Posic -> Tablero -> Bool
ocupado p t = getval p t /= Nothing

count :: Eq a => a -> [a] -> Int
count x = length . filter (x==)

bestplay :: Tablero -> Tablero
bestplay t = playy (bestpos t) t

bestpos :: Tablero -> Posic
bestpos t = fst $ foldl maxpos ((0,0),0) (evaltablero t)

maxpos::(Posic,Vreal) -> (Posic,Vreal) -> (Posic,Vreal)
maxpos (p1,v1) (p2,v2) = if (v1>v2) then (p1,v1) else (p2,v2)

evaltablero::Tablero -> [(Posic,Vreal)]
evaltablero t = map (\x -> (x,evalpos t x)) allpos

playy:: Posic -> Tablero -> Tablero
playy p t = putval Y p t

playx:: Posic -> Tablero -> Tablero
playx p t = putval X p t

allpos::[Posic]
allpos = [(x,y) | x <- [1..3], y <- [1..3]]

alltrios:: [TTT]
alltrios = [[(1,1),(1,2),(1,3)],[(2,1),(2,2),(2,3)],[(3,1),(3,2),(3,3)],
            [(1,1),(2,1),(3,1)],[(2,1),(2,2),(2,3)],[(3,1),(3,2),(3,3)],
            [(1,1),(2,2),(3,3)],[(1,3),(2,2),(3,1)]]

tttvacio:: [TTT]
tttvacio  = [[(0,0),(0,0),(0,0)]]

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
     
gano:: Valor -> Tablero -> Bool
gano x t = any (\y -> all ( \z -> z == Just x ) $ ttttoval t y) alltrios

empate :: Tablero -> Bool
empate t = not (gano X t) && not (gano Y t)

nofreePlace:: Tablero -> Bool
nofreePlace t = all (Nothing /=) $ map (\x -> getval x t) allpos

finished :: Tablero -> Bool
finished t = gano X t || gano Y t || nofreePlace t

resultado::Tablero -> String
resultado t
   | gano X t = "Gano X"
   | gano Y t = "Gano Y"
   | otherwise = "Empate"


readPos = do
  putStrLn "fila:"
  f<-getChar
  putStrLn "columna:"
  c<-getChar
  if elem f ['1'..'3'] && elem c ['1'..'3']
     then return (digitToInt f, digitToInt c)
     else print "valores invalidos" >> readPos

newStatePlayer:: Tablero -> IO (Tablero)
newStatePlayer t = do
    p <- readPos
    return $ playx p t

newState:: Tablero -> Valor -> IO (Tablero)
newState t v = do
   t <- if v == X then newStatePlayer t else return $ bestplay t
   print $ if v == Y then "player" else "maquina"
   print t
   if not (finished t) then newState t otro else return t
       where otro = if v == Y then X else Y

main = do
  putStrLn "Ta-Te-Ti Funcional"
  putStrLn " "
  putStrLn "X maquina Y persona"
  putStrLn " "
  putStrLn "empieza (S/N)?"
  ans<-getChar
--  putStrLn (if ans=='S' || ans=='s' then " juege" else " empiezo")
  t <- newState ttest $ if ans=='S' || ans=='s' then Y else X
  putStrLn (resultado t)


