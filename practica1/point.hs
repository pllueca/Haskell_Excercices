module Point where

--  Point  --

data Point = Point [Double]
             deriving (Show)
                      
-- retorna el valor de la cordenada del primer parametre
sel :: Int -> Point -> Double
sel x (Point l) = l !! x

-- retorna la dimensio del punt 
dim :: Point -> Int
dim (Point l) = length l


--f :: Point -> Point -> Int -> Int
f e1 e2 x 
  | (sel (x-1) e2) > (sel (x-1) e1) = 1
  | otherwise = 0

--comparaCoords :: Point -> Point -> [Int] -> [Int]
comparaCoords e1 e2 l = map (f e1 e2) l

-- donats dos punts i una llista de coordenades seleccionades retorna el numero de fill de e2 q li toca a e1
--child :: Point -> Point -> [Int] -> Int
child e1 e2 l =  valBin l2
                 where l2 = comparaCoords e1 e2 l
                     
-- rep una llista de 1 i 0, retorna el valor en decimal del nombre binari q representa
--valBin :: [Int] -> Int
valBin [x] = x
valBin l = 2 * (valBin $ init l) + last l

main =
  child p1 p3 l
  where
    p1 = Point [3.1, -1, 2.5]
    p2 = Point [2.1, 3.5, 6.5]
    p3 = Point [1.1, 1, 2.5]
    l = [1,3]
      
