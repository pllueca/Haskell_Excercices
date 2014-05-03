module Tauler where
import System.Console.ANSI
--data Color = Blue | Red
data Tipus = S|O 
           deriving (Eq, Show)
data Casella = Casella Color Tipus | B
             deriving (Eq,Show)

data Tauler = Tauler [[Casella]] 
              deriving (Show)
                       
tini :: Int -> Int -> Tauler
tini x y = Tauler (take x (cycle [take y (cycle [B])]))



get_casella :: Int -> Int -> Tauler -> Casella
get_casella x y (Tauler t) = c
  where
    c = ((t !! x) !! y)


num_sos_total :: Tauler -> Int
num_sos_total t = (sos_files t) -- + (sos_cols t) + (sos_diags t)

sos_files (Tauler []) = 0
sos_files (Tauler (f:fs)) = (sos_llista f) + (sos_files (Tauler fs) )

sos_llista :: [Casella] -> Int
sos_llista [] = 0
sos_llista ((Casella _ S):(Casella _ O):(Casella c S):l) = 1 + (sos_llista ((Casella c S):l))
                               

guanya_blau :: Tauler -> Bool
guanya_blau (Tauler t) = files || cols || diag
  where files = sos_blau_files t
        cols = False -- sos_blau_cols t
        diag = False --sos_blau_diag t

--guanya_vermell :: Tauler -> Bool
--guanya_vermell (Tauler t) = (files || cols || diag)
--  where files = sos_vermell_files t
--        cols = sos_vermell_cols t
--        diag = sos_vermell_diag t
        
sos_blau_files [] = False
sos_blau_files (l:ls)
  | sos_blau_llista l = True
  | otherwise = sos_blau_files ls
                
sos_blau_llista [] = False
sos_blau_llista ((Casella Blue S):(Casella Blue O):(Casella Blue S):l) = True
sos_blau_llista (_:l) = sos_blau_llista l
        
                        
                        
                        
                        
-- IO --

print_casella (Casella c t) = do
  setSGR [SetColor Foreground Vivid c]
  putStr (show t)
  setSGR []
  putStr " "
  
print_casella B = do
  setSGR [SetColor Foreground Vivid Green]
  putStr "_"
  setSGR []
  putStr " "
  

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