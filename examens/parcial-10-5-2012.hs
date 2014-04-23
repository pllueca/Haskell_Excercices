-- Problema 1
-- usant lambda expresions, indiqui si el primer nombre es el doble del 2n
esDob x y = (\z -> 2 * z) x == y

-- Problema 2
-- elimina els elements de la llista q son divisibles x algun elemnt anterior
elimDiv :: [Int] -> [Int]
elimDiv [] = []
elimDiv (x:xs) = [x] ++ elimDiv l2
  where l2 = filter (nodiv x) xs
        
nodiv x y = (mod y x) /= 0
      
-- Problema 3
infGen :: (Int -> Int) -> [Int]
infGen f = iterate f 1

getN :: (Int -> Int) -> Int -> Int
getN f n = (infGen f) !! n


-- Problema 4
-- 4.1
data Graph = G [Int] (Int -> [Int])

instance Eq Graph where
  (G l1 f1) == (G l2 f2) =  
    
