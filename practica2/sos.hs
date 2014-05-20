import Tauler
import System.Random
import System.Console.ANSI


readInt :: IO Int
readInt = do
  l <- getLine
  return (read l)
  
-- Llegeix un Tipus de Casella (S o O)
readTip :: IO Tipus
readTip = do
  l <- getLine
  if l == "S" then do
    return S
    else do    
    return O
  
color :: Int -> Color
color 1 = Blue
color 2 = Red

tipus :: Int -> Tipus
tipus 1 = S
tipus 2 = O


nextPlayer :: Int -> Int
nextPlayer 1 = 2
nextPlayer 2 = 1

-- Retorna una posicio buida al tauler (escollida aleatoriament)
getMovRand :: Tauler -> IO (Int,Int)
getMovRand t = do
    rand <- randomRIO (0, (length l_movs -1))
    return (l_movs !! rand)
  where l_movs = get_caselles_buides t
        
getTipusRand :: IO Tipus
getTipusRand = do
  r <- randomRIO (1,2)
  return (tipus r)

        

fes_moviment :: Tauler -> Color -> Tipus -> (Int,Int) -> Tauler
fes_moviment t c tp (x,y) = set_casella x y t (Casella c tp)

nextTiradaCPUCPU :: Partida -> Int -> Int -> Int -> IO()
nextTiradaCPUCPU part@(Game t p1 p2) pOld pNew j = do
  print_partida part
  if pNew > pOld then do
    putStrLn ("Sos! Torna a tirar el jugador "++ (show j));
    if j == 1 then do
      tirada (Game t (p1 + (pNew - pOld)) p2) 1
      else do
      tirada (Game t p1 (p2 + (pNew - pOld))) 2
    else do
    tirada (Game t p1 p2) (nextPlayer j)

nextTiradaHumCPU :: Partida -> Int -> Int -> Int -> IO()
nextTiradaHumCPU part@(Game t p1 p2) pOld pNew j = do
    print_partida part
    if pNew > pOld then do
      putStrLn ("Sos! Torna a tirar el jugador "++ (show j));
      if j == 1 then do
        tiradaHum (Game t (p1 + (pNew - pOld)) p2) 1
        else do
        tiradaCPU (Game t p1 (p2 + (pNew - pOld))) 2
      else
      if j == 2 then do
        tiradaHum (Game t p1 p2) 1
        else
        tiradaCPU (Game t p1 p2) 2
  
  
-- La CPU fa un moviment i es torna a cridar, si no ha acabat la partida. Per partides CPU vs CPU
tirada :: Partida -> Int -> IO()
tirada part@(Game t p1 p2) j = do
  if tauler_ple t then do 
    end_partida part
    else do    
    m <- (getMovRand t);
    c <- getTipusRand
    let t2 = (fes_moviment t (color j) c m)
    let pNew = (num_sos_total t2)
    nextTiradaCPUCPU (Game t2 p1 p2) pOld pNew j
  where
   pOld = p1 + p2
   
-- La CPU fa un moviment i es torna a cridar, si no ha acabat la partida. Per partides Huma vs CPU
tiradaCPU :: Partida -> Int -> IO()
tiradaCPU part@(Game t p1 p2) j = do
  if tauler_ple t then do 
    end_partida part
    else do    
    m <- (getMovRand t);
    c <- getTipusRand
    let t2 = (fes_moviment t (color j) c m)
    let pNew = (num_sos_total t2)
    nextTiradaHumCPU (Game t2 p1 p2) pOld pNew j
  where
   pOld = p1 + p2
   

-- Llegeix un moviment (el del jugador) i l'efectua
tiradaHum :: Partida -> Int -> IO()
tiradaHum part@(Game t p1 p2) j = do
  if tauler_ple t then do
    end_partida part
    else 
    do
      let cb = get_caselles_buides t
      putStrLn(show cb)
      putStrLn("Fila ?")
      x <- readInt;
      putStrLn("Columna ?")
      y <- readInt;
      putStrLn("Tipus ?")
      tip <- readTip;
      let t2 = fes_moviment t (color j) tip (x,y);
      let pNew = (num_sos_total t2);
      nextTiradaHumCPU (Game t2 p1 p2) pOld pNew j
  where
    pOld = p1 + p2
    
  
-- final de la partida, diu les puntuacions i el guanyador
end_partida :: Partida -> IO()
end_partida (Game t p1 p2) 
  | p1 < p2 = 
    do
      putStrLn ("Guanya el jugador 2!")
      print_partida (Game t p1 p2)
  | p1 > p2 = 
    do
      putStrLn ("Guanya el jugador 1!")
      print_partida (Game t p1 p2)
  | otherwise = 
    do
      putStrLn ("Empat!")
      print_partida (Game t p1 p2)
      
    

partida_CPU_CPU :: Int -> Int -> Int -> Int -> IO ()
partida_CPU_CPU dif1 dif2 nc nr = do 
  tirada (creaPartida nc nr) 1
  
partida_Huma_CPU :: Int -> Int -> Int -> IO()
partida_Huma_CPU dif nc nr = do
  tiradaHum (creaPartida nc nr) 1
  
      

main = do
  putStrLn ("SOS!")
  putStrLn ("Nombre de files?")
  nrows <- readInt;
  putStrLn ("Nombre de columnes?")
  ncols <- readInt;
  putStrLn("Com sera la partida?\n1: Jugador contra CPU\n2: CPU contra CPU")
  t <- readInt;
  if t == 1 then do
    putStrLn ("Estrategia usada per CPU?\n1: Random\n2: Llest");
    cpu <- readInt;
    partida_Huma_CPU cpu nrows ncols
    else do
    putStrLn ("Estrategia usada per CPU1?\n1: Random\n2: Llest");
    cpu1 <- readInt;
    putStrLn ("Estrategia usada per CPU2?\n1: Random\n2: Llest");
    cpu2 <- readInt;
    partida_CPU_CPU cpu1 cpu2 nrows ncols
  return ()
  
  

  