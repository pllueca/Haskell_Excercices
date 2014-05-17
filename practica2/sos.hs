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

--getMovRand :: Tauler -> (Int,Int)
getMovRand t = l_movs !! 0
  where l_movs = get_caselles_buides t
--        rand = getStdRandom (randomR (0, (length l_movs)))
        
fes_moviment :: Tauler -> Color -> Tipus -> (Int,Int) -> Tauler
fes_moviment t c tp (x,y) = set_casella x y t (Casella c tp)

-- fa un moviment i es torna a crida, si no ha acabat la partida 
tirada :: Partida -> Int -> IO()
tirada (Game t p1 p2) j = do
  print_tauler t
  if tauler_ple t then do 
    end_partida (Game t p1 p2)
  else do
  if pNew > pOld then 
                    if j == 1 then
                      tirada (Game t2 (p1 + (pNew - pOld)) p2) 1
                    else
                      tirada (Game t2 p1 (p2 + (pNew - pOld))) 2
                  else
                    tirada (Game t2 p1 p2) (nextPlayer j)
  where
    m = getMovRand t    
    pOld = p1 + p2
    pNew = num_sos_total t2
    t2 = fes_moviment t (color j) S m
    
end_partida :: Partida -> IO()
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
    

partida_CPU_CPU :: Int -> Int -> Int -> Int -> IO ()
partida_CPU_CPU dif1 dif2 nc nr = do 
  tirada (creaPartida nc nr) 1
  
  
      

main = do
  putStrLn ("SOS!")
  putStrLn ("Nombre de files?")
  nrows <- readInt;
  putStrLn ("Nombre de columnes?")
  ncols <- readInt;
  putStrLn ("Estrategia usada per CPU1?\n1: Random\n2: Llest");
  cpu1 <- readInt;
  putStrLn ("Estrategia usada per CPU2?\n1: Random\n2: Llest");
  cpu2 <- readInt;
  partida_CPU_CPU cpu1 cpu2 nrows ncols
  return ()
  
  

  