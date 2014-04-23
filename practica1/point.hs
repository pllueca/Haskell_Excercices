module Point where
--  Point  --
class Point p where
-- retorna el valor de la cordenada del primer parametre
  sel :: Int -> p -> Double

-- retorna la dimensio del punt 
  dim :: p -> Int

-- donats dos punts i una llista de coordenades seleccionades retorna el numero de fill de e2 q li toca a e1
  child :: p -> p -> [Int] -> Int

-- retorna la distancia entre dos punts
  dist :: p -> p -> Double

-- crea un punt a partir duna llista
  listToPoint :: Double -> p
