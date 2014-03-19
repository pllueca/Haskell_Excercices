--   FUNCIONS AUXILIARS
-- inserta un element a una llista ordenada
insert :: [Int] -> Int -> [Int]
insert [] x = [x]
insert [x] y 
  | x < y = [x,y]
  | otherwise = [y,x]
insert (x:(n:xs)) y 
  | (y < x) = [y] ++ (x:(n:xs))
  | (x < y) && (y < n) = [x,y,n] ++ xs
  | otherwise = [x] ++ insert (n:xs) y
                
-- elimina la primera ocurrencia de un element a una llista
remove :: [Int] -> Int -> [Int]
remove [] y = []
remove (x:xs) y 
  | x == y    = xs
  | otherwise = [x] ++ (remove xs y)
                                
                
-- retorna l'element minim d'una llista
minList :: [Int] -> Int
minList [x] = x
minList (x:xs) 
  | x < m = x
  | otherwise = m
  where m = minList xs
        
insertAll :: [Int] -> [Int] -> [Int]
insertAll [] l = l
insertAll (x:xs) l = insertAll xs (insert l x)

-- aplana una llista de llistes, transformantla en una llista
-- d'elements
flatten :: [[Int]] -> [Int]
flatten [] = []
flatten (x:xs) = 
  x ++ x2
  where x2 = flatten xs
        
-- donades dos llistes ordenades, la fusio de les dues
merge :: [Int] -> [Int] -> [Int]
merge [] [] = []
merge l [] = l
merge [] l = l
merge (x:xs) (y:ys)
  | x < y = [x] ++ merge xs (y:ys)
  | otherwise = [y] ++ merge (x:xs) ys

-- donada una llista retorna una llista q conte les llistes dun element
-- [1,2,3,4] -> [[1],[2],[3],[4]]
separa :: [Int] -> [[Int]]
separa [x] = [[x][]]
separa [x,y] = [[x][y]]
separa l = ((separa l1):separa l2)


-- ordenacio x insercio
isort :: [Int] -> [Int]
isort l = insertAll l []

--ordenacio x seleccio
ssort :: [Int] -> [Int]
ssort [x] = [x]
ssort l = [x] ++ ssort l2
  where x = minList l
        l2 = remove l x



msort :: [Int] -> [Int]


        