import Data.Char
--import Control.Monad.Loops

data Fila a =  Row  a a a deriving (Eq,Show)

data Matriz a = Matrix (Fila a) (Fila a) (Fila a) 

instance Show a => Show (Matriz a) where
   show (Matrix f1 f2 f3) = show f1 ++ "\n" ++ show f2 ++ "\n" ++ show f3

mtest = Matrix (Row 1 2 3) (Row 4 5 6) (Row 7 8 9)

fila:: Int -> Matriz a -> Fila a
fila n (Matrix f1 f2 f3)
   | n == 1 = f1
   | n == 2 = f2
   | n == 3 = f3

putrow:: a -> Int -> Fila a -> Fila a
putrow x c (Row a1 a2 a3) 
   | c == 1 = Row x a2 a3
   | c == 2 = Row a1 x a3
   | c == 3 = Row a1 a2 x

putval:: a -> Int -> Int -> Matriz a -> Matriz a
putval x f c (Matrix f1 f2 f3) 
   | f == 1 = Matrix (putrow x c f1) f2 f3
   | f == 2 = Matrix f1 (putrow x c f2) f3
   | f == 3 = Matrix f1 f2 (putrow x c f3) 

getvalrow:: Int -> Fila a -> a
getvalrow c (Row a1 a2 a3) 
   | c == 1 = a1
   | c == 2 = a2
   | c == 3 = a3

getval:: Int -> Int -> Matriz a -> a
getval f c (Matrix f1 f2 f3) 
   | f == 1 = getvalrow c f1
   | f == 2 = getvalrow c f2
   | f == 3 = getvalrow c f3 

allpos::[(Int,Int)]
allpos = [(x,y) | x <- [1..3], y <- [1..3]]

data TTT = V | X | Y deriving (Eq,Show)

type Posic = (Int,Int)

tablero = Matrix (Row V V V) (Row V V V) (Row V V V)

getposval::Posic -> Matriz TTT -> TTT
getposval (f,c) = getval f c 

getRowVals:: Int -> Matriz TTT -> [TTT]
getRowVals r m = [getval r 1 m, getval r 2 m, getval r 3 m]   

getColVals:: Int -> Matriz TTT -> [TTT]
getColVals c m = [getval 1 c m, getval 2 c m, getval 3 c m]   

getDiagPpal:: Matriz TTT -> [TTT]
getDiagPpal m = [getval 1 1 m, getval 2 2 m, getval 3 3 m]   

getDiagSec:: Matriz TTT -> [TTT]
getDiagSec m = [getval 1 3 m, getval 2 2 m, getval 3 1 m]   

mydiags:: Posic -> Matriz TTT -> [TTT]
mydiags (f,c) m
   | f == 2 && c == 2 = getDiagPpal m ++ getDiagSec m
   | f == 1 && c == 1 = getDiagPpal m 
   | f == 3 && c == 3 = getDiagPpal m 
   | f == 1 && c == 3 = getDiagSec m
   | f == 3 && c == 1 = getDiagSec m
   | otherwise = []


playvalue:: TTT -> TTT -> Int
playvalue yo t
   | yo == t  = 2
   | t == V   = 1
   | otherwise  = 0

valorize::TTT -> [TTT] -> [Int]
valorize yo vals = map (playvalue yo) vals

valpos:: TTT -> Posic -> Matriz TTT -> Int
valpos yo p m = foldl (+) 0 $ valorize yo myvalues
       where myvalues =  (getColVals (snd p) m) ++ (getRowVals (fst p) m) ++ mydiags p m

allposValues :: TTT -> Matriz TTT -> [(Posic,Int)]
allposValues yo t = map (\x -> (x , valpos yo x t)) allpos

allTrios:: Matriz TTT -> [[TTT]]
allTrios m = (getDiagPpal m) : (getDiagSec m) : ((map (\x -> getColVals x m) [1..3]) ++ (map (\x -> getRowVals x m) [1..3]))

availmoves:: TTT -> Matriz TTT -> [(Posic,Int)]
availmoves yo t = filter (\x -> getposval (fst x) t == V ) $ allposValues yo t

bestmove:: TTT -> Matriz TTT -> Posic
bestmove yo t = fst $ foldl (\x y -> mymax x y) ((0,0),0) $ availmoves yo t
      where mymax x y = if snd x >= snd y then x else y


play:: TTT -> Posic -> Matriz TTT -> Matriz TTT
play yo p m = putval yo (fst p) (snd p) m 

playbest:: TTT -> Matriz TTT -> Matriz TTT
playbest yo t = play yo (bestmove yo t) t 

gano:: TTT -> Matriz TTT -> Bool
gano a m = any (\x -> all (a==) x) $ allTrios m 

empate :: Matriz TTT -> Bool
empate m = not (gano X m) && not (gano Y m)

nofreePlace:: Matriz TTT -> Bool
nofreePlace m = all (V /=) $ map (\x -> getposval x m) allpos

finished :: Matriz TTT -> Bool
finished m = gano X m || gano Y m || nofreePlace m

resultado::Matriz TTT -> String
resultado m
   | gano X m = "Gano X"
   | gano Y m = "Gano Y"
   | otherwise = "Empate"

readPos = do
  putStrLn "fila:"
  f<-getChar
  putStrLn "columna:"
  c<-getChar
  if elem f ['1'..'3'] && elem c ['1'..'3']
     then return (digitToInt f, digitToInt c)
     else print "valores invalidos" >> readPos

newStatePlayer:: Matriz TTT -> IO (Matriz TTT)
newStatePlayer t = do
    p <- readPos
    return $ play Y p t

newState:: Matriz TTT -> TTT -> IO (Matriz TTT) 
newState t q = do
   t <- if q == Y then newStatePlayer t else return $ playbest X t
   print $ if q == Y then "player" else "maquina" 
   print t
   if not (finished t) then newState t otro else return t
       where otro = if q == Y then X else Y

main = do
  putStrLn "Ta-Te-Ti Funcional"
  putStrLn " "
  putStrLn "X maquina Y persona"
  putStrLn " "
  putStrLn "empieza (S/N)?"
  ans<-getChar
--  putStrLn (if ans=='S' || ans=='s' then " juege" else " empiezo")
  t <- newState tablero $ if ans=='S' || ans=='s' then Y else X 
  putStrLn (resultado t)
