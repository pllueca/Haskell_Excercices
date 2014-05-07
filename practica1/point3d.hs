--  Point  --
-- Excercici 1
class Point p where
  sel :: Int -> p -> Double
  dim :: p -> Int
  child :: p -> p -> [Int] -> Int
  cmp :: p -> p -> Int -> Int
  dist :: p -> p -> Double
  listToPoint :: [Double] -> p
  ptrans :: [Double] -> p -> p
  pscale :: Double -> p -> p


-- Point3d --
-- Excercici 2
data Point3d = Point3d [Double] deriving (Eq)
  
instance Show Point3d where
  show (Point3d [d1,d2,d3]) = "(" ++ (show d1) ++ "," ++ (show d2) ++ "," ++ (show d3) ++")"
  
instance Point Point3d where
  sel x (Point3d l) = l !! x                      
  dim p = 3
  cmp e1 e2 x = if (sel (x-1) e2) > (sel (x-1) e1) then 1
                else 0
  
  child e1 e2 l = valBin l2
    where l2 = map (cmp e1 e2) l
          
          
  dist (Point3d [x1,y1,z1]) (Point3d [x2,y2,z2]) = sqrt (((x2 - x1) ** 2) + 
                                                   ((y2 - y1) ** 2) + ((z2 - z1) ** 2) )
  listToPoint l = (Point3d l)
  ptrans lt (Point3d l) = listToPoint $ zipWith (+) l lt
  pscale x (Point3d l) = listToPoint $ map (* x) l
  
  



-- Kd2nTree --
-- Excercici 3
data Kd2nTree p = Node p [Int] [Kd2nTree p] | Empty

-- com els kd2ntrees son arbres de cerca, realment representen un conjunt de punts, aixi que no ens importa que tinguin la 
-- mateixa forma, nomes els mateixos elements
instance (Point p, Eq p) => Eq (Kd2nTree p) where
  Empty == Empty = True
  t1 == t2 = and [(contains_all l1 t2), (size t1) == (size t2)]
    where 
      l1 = get_all t1

instance Show p => Show (Kd2nTree p) where
  show Empty = ""
  show (Node p1 l1 lfills) = (show p1) ++ " " ++ (show l1) ++ "\n" 
                             ++ showChild lfills 0 0
    where showChild :: Show p => [Kd2nTree p] -> Int -> Int -> String
          showChild [] _ _ = ""
          showChild (Empty:xs) n m = (showChild xs (n+1) m)
          showChild ((Node p lc ff):xs) n m = (take (m*4) (cycle " ")) ++ 
                                              " <" ++ (show n) ++ "> "  
                                              ++ (show p) ++ " " ++ (show lc) ++ "\n" ++
                                              (showChild ff 0 (m+1)) ++
                                              (showChild xs (n+1) m) 

  
-- Excercici 4
-- retorna el resultat d'inserir un punt al kd2ntree
insert :: Point p => Kd2nTree p -> p -> [Int] -> Kd2nTree p
insert Empty p l = Node p l (replicate x Empty)
  where x =  2 ^ length l
insert (Node p1 lcoords lfills) p2 lcoord2 = Node p1 lcoords (fillsAnt ++ [fill] ++ fillsSeg)
  where fillsAnt = take n lfills
        fillsSeg = drop (n+1) lfills 
        fill = insert (head (drop n lfills)) p2 lcoord2
        n = child p1 p2 lcoords

-- construeix un kd2ntree a partir duna llista de parells (punt, llista de coordenades)
build :: (Eq p, Point p) => [(p, [Int])] -> Kd2nTree p
build [] = Empty
build [(p1, l1)] = Node p1 l1 (replicate x Empty)
  where x =  2 ^ length l1
build (p1:xs) = insertlist t1 xs
                where t1 = build [p1]

-- Excercici 5
get_all :: Point p => Kd2nTree p -> [(p, [Int])]
get_all Empty = []
get_all (Node p ll []) = [(p, ll)]
get_all (Node p ll (x:xs)) = (get_all x) ++ (get_all (Node p ll xs))

-- Excercici 6
remove :: (Point p, Eq p) => Kd2nTree p -> p -> Kd2nTree p
remove t@(Node p lc lf) pr 
  | p == pr = if esfulla t then
                Empty
                else build (get_childs t)
  | otherwise = (Node p lc (linis ++ [remove f1 pr] ++ llasts))
  where 
    linis = take x lf
    llasts = drop (x+1) lf
    f1 = head $ drop x lf 
    x = child p pr lc


-- Excerici 7
contains :: (Point p, Eq p) => Kd2nTree p -> p -> Bool
contains Empty p = False
contains (Node p ll lfills) p1
  | (p == p1) = True
  | otherwise = contains af p1
  where        
    af = lfills !! x
    x = child p p1 ll
    
-- Excercici 8
nearest :: (Point p, Eq p) => Kd2nTree p -> p -> p
nearest t1@(Node q lc lf) p 
  | p == q = p
  | otherwise = nearest_child lf p q
  where
    nearest_child [] p q = q  -- q es el mes proper fins ara
    nearest_child (f:fs) p q 
      | (esbuit f) = nearest_child fs p q
      | otherwise = if (dist p q) > (dist p q1) then nearest_child fs p q1
                    else nearest_child fs p q
      where
        q1 = nearest f p

-- Excercici 9
kdmap :: (Point p, Point q) => (p -> q)  -> Kd2nTree p -> Kd2nTree q
kdmap f Empty = Empty
kdmap f (Node p1 lc lf) = (Node p2 lc lf2)
  where
    p2 = f p1
    lf2 = map (kdmap f) lf 
    
translation :: (Point p) => [Double] -> Kd2nTree p -> Kd2nTree p
translation lt t = kdmap (ptrans lt) t

scale :: (Point p) => Double -> Kd2nTree p -> Kd2nTree p
scale x t = kdmap (pscale x) t

-- Funcions Auxiliars
    
-- rep una llista de 1 i 0, retorna el valor en decimal del nombre binari q representa
valBin :: [Int] -> Int
valBin [x] = x
valBin l = 2 * (valBin $ init l) + last l

-- retorna el nombre de nodes de l'arbre
size :: (Point p) => Kd2nTree p -> Int
size t = length $ get_all t

-- retorna si l'arbre conte tots els punts de la llista (ignora les coordenades de seleccio)
contains_all :: (Point p, Eq p) => [(p,[Int])] -> Kd2nTree p -> Bool
contains_all [] t = True
contains_all ((p1,_):xs) t2
    | (contains t2 p1) = contains_all xs t2
    | otherwise = False

-- retorna tots els punts que contenen els fills de l'arbre (no inclou l'arrel)
get_childs :: Point p => Kd2nTree p -> [(p, [Int])]
get_childs Empty = []
get_childs (Node _ ll []) = []
get_childs (Node p ll (x:xs)) = (get_all x) ++ (get_childs (Node p ll xs))

-- insereix una llista de punts a un arbre
insertlist :: Point p => Kd2nTree p -> [(p, [Int])] -> Kd2nTree p
insertlist t [] = t
insertlist t ((p1, l1):xs) = insertlist (insert t p1 l1) xs
    
-- retorna si l'arbre es buit o no
esbuit :: (Point p) => Kd2nTree p -> Bool
esbuit Empty = True
esbuit t1 = False

-- retorna si l'arbre es una fulla (ssi tots els seus fills son Empty)
esfulla :: (Point p) => Kd2nTree p -> Bool
esfulla Empty = False
esfulla (Node _ _ lf) = and (map (esbuit) lf)


-- constructors per fer proves
buildIni :: Kd2nTree Point3d
buildIni = build [(Point3d [3.0, -1.0, 2.1], [1, 3]), 
                  (Point3d [3.5, 2.8, 3.1], [1, 2]), 
                  (Point3d [3.5, 0.0, 2.1], [3]), 
                  (Point3d [3.0, -1.7, 3.1], [1, 2, 3]),
                  (Point3d [3.0, 5.1, 0.0], [2]), 
                  (Point3d [1.5, 8.0, 1.5], [1]), 
                  (Point3d [3.3, 2.8, 2.5], [3]), 
                  (Point3d [4.0, 5.1, 3.8], [2]),
                  (Point3d [3.1, 3.8, 4.8], [1, 3]), 
                  (Point3d [1.8, 1.1, -2.0], [1, 2])
                 ]
           
exampleSet :: Kd2nTree Point3d
exampleSet = build [(Point3d [0,0,0], [3]), 
                    (Point3d [0.5, -0.5, 1.0], [1]), 
                    (Point3d [0.7, -1, 3], [1,2]), 
                    (Point3d [0,0,-3], [1,3])
                   ]
