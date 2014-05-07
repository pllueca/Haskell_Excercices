import Tauler
import System.Random

readInt :: IO Int
readInt = do
  l <- getLine
  return (read l)
  
  
Data Moviment = Mov Int Int Tipus

color :: Int -> Color
color 1 = Blue
color 2 = Red

-- rep un tauler i un jugador i retorna el moviment que efectuara
estrategia :: Tauler -> Int -> Moviment

-- juga la CPU, el jugador
tiraCPU :: Tauler -> Color -> Tauler
--tiraCPU t c = actualitza t m
          
-- fa un moviment i es torna a crida, si no ha acabat la partida 
partida :: Tauler -> Int -> Int -> Int -> Tauler
partida t p1 p2 pAct  
  | 

main = do
  putStrLn ("SOS!")
  putStrLn ("Nombre de files?")
  nrows <- readInt
  putStrLn ("Nombre de columnes?")
  ncols <- readInt
  
  putStrLn("Tipus de joc?\n1: Jugador vs CPU\n2: CPU vs CPU")
  m <- readInt
  
  if m == 2 then do
    putStrLn ("Estrategia usada per CPU1?\n1: Random\n2: Llest")
    cpu1 <- readInt
    putStrLn ("Estrategia usada per CPU2?\n1: Random\n2: Llest")
    cpu2 <- readInt    
    
    else do
        putStrLn ("Estrategia usada per CPU?\n1: Random\n2: Llest")
        cpu <- readInt
        
  let t = tiniSos nrows ncols
  print_tauler t
  
  return m
