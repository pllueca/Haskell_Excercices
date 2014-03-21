--   FUNCIONS AUXILIARS

-- retorna la longitud d'una llista
myLength :: [Int] -> Int
myLength [] = 0
myLength (x:xs) = 1 + myLength xs

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


-- donades dos llistes ordenades, la fusio de les dues
merge :: [Int] -> [Int] -> [Int]
merge [] [] = []
merge l [] = l
merge [] l = l
merge (x:xs) (y:ys)
  | x < y = [x] ++ merge xs (y:ys)
  | otherwise = [y] ++ merge (x:xs) ys


-- ordenacio x insercio
isort :: [Int] -> [Int]
isort l = insertAll l []

--ordenacio x seleccio
ssort :: [Int] -> [Int]
ssort [x] = [x]
ssort l = [x] ++ ssort l2
  where x = minList l
        l2 = remove l x


--retorna la primera meitat de la llista
firstH :: [Int] -> Int -> [Int]
firstH l n = take a l
  where a = quot n 2

lastH :: [Int] -> Int -> [Int]
lastH l n = drop a l
  where a = quot n 2


-- merge sort 
msort :: [Int] -> [Int]
msort [x] = [x]
msort [x,y] = merge [x] [y]
msort l = merge (msort l1) (msort l2)
  where l1 = firstH l n
        l2 = lastH l n
        n = myLength l