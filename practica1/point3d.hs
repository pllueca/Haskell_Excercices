--  Point  --
class Point p where
-- retorna el valor de la cordenada del primer parametre
  sel :: Int -> p -> Double

-- retorna la dimensio del punt 
  dim :: p -> Int

-- donats dos punts i una llista de coordenades seleccionades retorna el numero de fill de e2 q li toca a e1
  child :: p -> p -> [Int] -> Int
  
  cmp :: p -> p -> Int -> Int 

-- retorna la distancia entre dos punts
  dist :: p -> p -> Double

-- crea un punt a partir duna llista
  listToPoint :: [Double] -> p



-- Point3d --
data Point3d = Point3d [Double]
               deriving (Show, Eq)
  
(==) :: (Eq Point3d) => Point3d -> Point3d -> Bool
p1 == p2 = 0 == dist p1 p2


instance Point Point3d where
  sel x (Point3d l) = l !! x
                      
  dim p = 3
         
  cmp e1 e2 x = if (sel (x-1) e2) > (sel (x-1) e1) then 1
                else 0
                            
  child e1 e2 l = valBin l2
    where l2 = map (cmp e1 e2) l
         
  dist (Point3d [x1,y1,z1]) (Point3d [x2,y2,z2]) = sqrt ((x2 - x1) ** 2) + ((y2 - y1) ** 2) + ((z2 - z1) ** 2) 
         
  listToPoint l = (Point3d l)


  


-- rep una llista de 1 i 0, retorna el valor en decimal del nombre binari q representa
valBin :: [Int] -> Int
valBin [x] = x
valBin l = 2 * (valBin $ init l) + last l


-- Kd2nTree --
data Kd2nTree = Kd2nTree [([Double], [Int])]
                deriving (Show, Eq)
                         
--instance Eq Kd2nTree where
--  t1 == t2 = 