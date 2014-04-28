--  Point  --
class Point p where
  sel :: Int -> p -> Double
  dim :: p -> Int
  child :: p -> p -> [Int] -> Int
  cmp :: p -> p -> Int -> Int
  dist :: p -> p -> Double
  listToPoint :: [Double] -> p


-- Point3d --
data Point3d = Point3d [Double]
  
instance Eq Point3d where
  e1 == e2 = 0.0 == dist e1 e2  
instance Show Point3d where
  show (Point3d [d1,d2,d3]) = "(" ++ (show d1) ++ "," ++ (show d2) ++ "," ++ (show d3) ++")"
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
data Kd2nTree p = Node p [Int] [Kd2nTree p] | Empty

-- com els kd2ntrees son arbres de cerca, realment representen un conjunt de punts, aixi que no ens importa que tinguin la 
-- mateixa forma, nomes els mateixos elements
--instance Eq p => Eq (Kd2nTree p) where
--  (Kd2nTree l1) == (Kd2nTree l2) = 


instance Show p => Show (Kd2nTree p) where
--  show :: Point p => Kd2nTree p -> string
  show (Node p1 l1 lfills) = (show p1) ++ " " ++ (show l1) ++ "\n" 
                             ++ showChild lfills 0 0
    where showChild :: Show p => [Kd2nTree p] -> Int -> Int -> String
          showChild [] _ _ = ""
          showChild (Empty:xs) n m = (showChild xs (n+1) m)
          showChild ((Node p lc ff):xs) n m = (take (m*4) (cycle " ")) ++ "<" ++ (show n) ++ ">" 
                                              ++ (show p) ++ " " ++ (show lc) ++ "\n" ++
                                              (showChild xs (n+1) m) ++
                                              (showChild ff 0 (m+1))
  
-- retorna el resultat d'inserir un punt al kd2ntree
insert :: Point p => Kd2nTree p -> p -> [Int] -> Kd2nTree p
insert Empty p l = Node p l (replicate x Empty)
  where x =  2 ^ length l
insert (Node p1 lcoords lfills) p2 lcoord2 = Node p1 lcoords (fillsAnt ++ [fill] ++ fillsSeg)
  where fillsAnt = take n lfills
        fillsSeg = drop (n+1) lfills 
        fill = insert (head (drop n lfills)) p2 lcoord2
        n = child p1 p2 lcoords

build :: Point p => [(p, [Int])] -> Kd2nTree p
build [] = Empty
build [(p1, l1)] = Node p1 l1 (replicate x Empty)
  where x =  2 ^ length l1
build (p1:xs) = insertlist t1 xs
                where t1 = build [p1]

insertlist :: Point p => Kd2nTree p -> [(p, [Int])] -> Kd2nTree p
insertlist t [] = t
insertlist t ((p1, l1):xs) = insertlist (insert t p1 l1) xs


exampleSet :: Kd2nTree Point3d
exampleSet = build [(Point3d [0,0,0], [3]), (Point3d [0.5, -0.5, 1.0], [1]), (Point3d [0.7, -1, 3], [1,2]), (Point3d [0,0,-3], [1,3])]