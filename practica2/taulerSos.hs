module Tauler where
import System.Console.ANSI

data Tipus = S|O 
           deriving (Eq, Show)
                    
data Casella = Casella Color Tipus | B
             deriving (Eq,Show)

data Tauler = Tauler [[Casella]] 
              deriving (Show)
                       
data Partida = Game Tauler Int Int



-- Partida
creaPartida :: Int -> Int -> Partida
creaPartida x y = (Game (tini x y) 0 0)

-- Funcions Tauler

get_casella :: Int -> Int -> Tauler -> Casella
get_casella x y (Tauler t) = c  
  where
    c = ((t !! x) !! y)
    
set_casella :: Int -> Int -> Tauler -> Casella -> Tauler
set_casella x y (Tauler t) c = (Tauler (finis ++ [fn] ++ flasts))
  where finis = take x t
        fa = head $ drop x t
        fn = (take y fa) ++ [c] ++ (drop (y+1) fa)
        flasts = drop (x + 1) t

-- Retorna si el tauler te alguna casella lliure
tauler_ple :: Tauler -> Bool
tauler_ple (Tauler []) = True
tauler_ple (Tauler (f:fs)) = if any (== B) f then False
                           else tauler_ple (Tauler fs)
                                
-- Retorna la llista de parells (x,y) que representen la posicio de les caselles buides
get_caselles_buides :: Tauler -> [(Int,Int)]
get_caselles_buides (Tauler t) = get_cas_empty t 0


get_cas_empty t x = if x >= length t then []
                    else (get_cas_empty_fila (t !! x) x 0) ++ (get_cas_empty t (x+1))

get_cas_empty_fila [] _ _ = []
get_cas_empty_fila (x:xs) i j 
  | x == B = [(i,j)] ++ get_cas_empty_fila xs i (j+1)
  | otherwise = get_cas_empty_fila xs i (j+1)
  

num_sos_total :: Tauler -> Int
num_sos_total t = (sos_files t) + (sos_cols t (num_cols t)) --  + (sos_diags t)

sos_files (Tauler []) = 0
sos_files (Tauler (f:fs)) = (sos_llista f) + (sos_files (Tauler fs) )


sos_cols t x 
  | x == 0 = sos_llista (get_columna t x)
  | otherwise = (sos_llista (get_columna t x)) + (sos_cols t (x - 1))
  
-- retorna la iessima columna de un tauler  
get_columna :: Tauler -> Int -> [Casella]
get_columna (Tauler []) _ = []
get_columna (Tauler (f:fs)) i = (take 1 $ drop i f) ++ (get_columna (Tauler fs) i)

sos_llista :: [Casella] -> Int
sos_llista [] = 0
sos_llista ((Casella _ S):(Casella _ O):(Casella c S):l) = 1 + (sos_llista ((Casella c S):l))
sos_llista (_:xs) = sos_llista xs
                        
                        
num_cols :: Tauler -> Int
num_cols (Tauler l) = length l

num_files :: Tauler -> Int
num_files (Tauler []) = 0
num_files (Tauler (x:xs)) = length x


-- IO --

print_partida (Game t j1 j2) = do
  print_tauler t
  

print_casella :: Casella -> IO()
print_casella (Casella c t) = do
  setSGR [SetColor Background Vivid c,
          SetColor Foreground Vivid White
         ]
  putStr (" " ++ (show t) ++ " ")
  setSGR []
  
print_casella B = do
  setSGR [SetColor Background Vivid Green]
  putStr "   "
  setSGR []
  putStr ""
  
print_tauler :: Tauler -> IO()
print_tauler (Tauler []) = do
  putStrLn ""
  
print_tauler (Tauler (x:xs)) = do
  print_fila x
  putStrLn ""
  print_tauler (Tauler xs)
  
print_fila [] = putStr ""
print_fila (c:cs) = do
  print_casella c
  print_fila cs
  
  
-- proves -- 
tini :: Int -> Int -> Tauler
tini x y = Tauler (take x (cycle [take y (cycle [B])]))

gameIni x y = Game (tini x y) 0 0

tiniSos x y = Tauler (take x (cycle [take y (cycle 
                                             ([(Casella Blue S)]++[(Casella Red O)])
                                            )]))
provasos = Tauler [[Casella Blue S, B],[Casella Red O, Casella Blue O], [Casella Blue S, B]]

              