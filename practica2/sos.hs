import Tauler
import System.Random

readInt :: IO Int
readInt = do
  l <- getLine
  return (read l)
  
  
Data Moviment = Mov Int Int Tipus

-- rep un tauler i un jugador i retorna el moviment que efectuara
estrategia :: Tauler -> Int -> Moviment

-- juga la CPU, el jugador
tiraCPU :: Tauler -> Color -> Tauler
tiraCPU t c = actualitza t m
  where m = 


main = do
  putStrLn ("SOS!")
  putStrLn ("Nombre de files?")
  nrows <- readInt
  putStrLn ("Nombre de columnes?")
  ncols <- readInt
  
  putStrLn("Tipus de joc?\n1: Jugador vs CPU\n2: CPU vs CPU")
  m <- readInt
  
  if m == 2 then do
    putStrLn ("Estrategia usada per CPU1?")
    
    putStrLn ("Estrategia usada per CPU2?")
    else do
        putStrLn ("Estrategia usada per CPU?")
  
  let t = tiniSos nrows ncols
  print_tauler t
  
  return m
