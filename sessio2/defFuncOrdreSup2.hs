countIf :: (Int -> Bool) -> [Int] -> Int
countIf f [] = 0
countIf f (x:xs) = if f x then 1 + countIf f xs 
                   else countIf f xs
                        
                        
myMap :: (a -> b) -> [a] -> [b]
myMap f [] = []
myMap f (x:xs) = [f x] ++ myMap f xs

-- retorna la llista de llistes resultant daplicar cada una de les funcions del segon parametre a tots els elements de el primer parametre
pam :: [Int] -> [Int -> Int] -> [[Int]]
pam [] l = []
pam l [] = []
pam l (f1:lf) = [myMap f1 l] ++ pam l lf


flatten :: [[Int]] -> [Int]
flatten [] = []
flatten l = foldl (++) [] l


pam2 :: [Int] -> [Int -> Int] -> [[Int]]
pam2 [] l = []
pam2 l [] = []
pam2 (x:xs) lf = [flatten (pam [x] lf)] ++ pam2 xs lf
  
filterFoldl :: (Int -> Bool) -> (Int -> Int -> Int) -> Int -> [Int] -> Int
filterFoldl fb fa x [] = x 
filterFoldl fb fa x (y:ys) = if fb y then filterFoldl fb fa (fa y x) ys
                             else filterFoldl fb fa x ys
                                  
-- donada una relacio entre enters, una llista i un enter inserta lenter on toca
insert :: (Int -> Int -> Bool) -> [Int] -> Int -> [Int] 
insert rel [] x = [x]
insert rel (x:xs) y 
  | rel x y = [x] ++ insert rel xs y 
  | otherwise = [y] ++ [x] ++ xs
                
                
insertionSort :: (Int -> Int -> Bool) -> [Int] -> [Int]
insertionSort rel [] = []
insertionSort rel [x] = [x]
insertionSort rel (x:xs) = insert rel (insertionSort rel xs) x