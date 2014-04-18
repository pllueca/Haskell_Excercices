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
