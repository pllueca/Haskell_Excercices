import Tauler
import System.Random
import System.Console.ANSI

readInt :: IO Int
readInt = do
  l <- getLine
  return (read l)
  
  
--Data Moviment = Mov Int Int Tipus

color :: Int -> Color
color 1 = Blue
color 2 = Red


nextPlayer :: Int -> Int
nextPlayer 1 = 2
nextPlayer 2 = 1
-- rep un tauler i un jugador i retorna el moviment que efectuara
--estrategia :: Tauler -> Int -> Moviment

-- juga la CPU, el jugador
--tiraCPU :: Tauler -> Color -> Tauler
--tiraCPU t c = actualitza t m

getMovRand :: Tauler -> Int -> 


-- fa un moviment i es torna a crida, si no ha acabat la partida 
tirada :: Partida -> Int -> Partida
tirada (Game t p1 p2) j 
  | tauler_ple t = (Game t p1 p2)
  | otherwise =   if pNew > pOld then 
                    if j == 1 then
                      tirada (Game t2 (p1 + (pNew - pOld)) p2 1)
                    else
                      tirada (Game t2 p1 (p2 + (pNew - pOld)) 2)
                  else
                    tirada (Game t2 p1 p2 (nextPlayer j))
  where
    m = getMovRand t j
    pOld = p1 + p2
    pNew = num_sos_total t2
    t2 = fes_moviment t m
    
--end_partida :: Partida 
end_partida (Game t p1 p2) 
  | p1 < p2 = 
    do
      putStrLn ("Guanya el jugador 2!")
      print_tauler t
  | p1 > p2 = 
    do
      putStrLn ("Guanya el jugador 1!")
      print_tauler t
  | otherwise = 
    do
      putStrLn ("Empat!")
      print_tauler t
    

--partida_CPU_CPU :: Int -> Int -> Partida
partida_CPU_CPU dif1 dif2 nc nr = do
  let p = creaPartida nc nr;
  
      

main = do
  putStrLn ("SOS!")
  putStrLn ("Nombre de files?")
  nrows <- readInt
  putStrLn ("Nombre de columnes?")
  ncols <- readInt
  
  putStrLn("Tipus de joc?\n1: Jugador vs CPU\n2: CPU vs CPU")
  m <- readInt
  
  if m == 2 then     
    do
      putStrLn ("Estrategia usada per CPU1?\n1: Random\n2: Llest");
      cpu1 <- readInt;
      putStrLn ("Estrategia usada per CPU2?\n1: Random\n2: Llest");
      cpu2 <- readInt;
      return $ partida
    else 
    do
    putStrLn ("Estrategia usada per CPU?\n1: Random\n2: Llest");
    cpu <- readInt;
    return $ partida
    
        
  let t = tiniSos nrows ncols
  print_tauler t
  
  return m
