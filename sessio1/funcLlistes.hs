myLength :: [Int] -> Int
myLength [] = 0
myLength (x:xs) = 1 + myLength xs


myMaximum :: [Int] -> Int
myMaximum [x] = x
myMaximum (x:xs) = max x (myMaximum xs)

sumIl :: [Int] -> Int
sumIl [x] = x
sumIl (x:xs) = x + sumIl xs


-- fromIntegral int->float
average :: [Int] -> Float
average l = fromIntegral s /  fromIntegral n  
  where
    s = sumIl l
    n = myLength l
  
                
reverseList :: [Int] -> [Int]
reverseList [x] = [x]
reverseList (x:xs) = l ++ [x]
  where l = reverseList xs

buildPalindrome :: [Int] -> [Int]
buildPalindrome [] = []
buildPalindrome l = ls ++ l
  where ls = reverseList l

isIn :: Int -> [Int] -> Bool
isIn x [] = False
--isIn x (x:xs) = True
isIn x (y:xs)
  | x == y = True
  | otherwise = (isIn x xs)

removeE :: [Int] -> Int -> [Int]
removeE [] y = []
removeE (x:xs) y 
  | x == y    = removeE xs y
  | otherwise = [x] ++ (removeE xs y)


-- remove x y l
remove :: [Int] -> [Int] -> [Int]
remove l [] = l
remove [] _ = []
remove l (x:xs) = remove (removeE l x) xs

-- aplana una llista de llistes, transformantla en una llista
-- d'elements
flatten :: [[Int]] -> [Int]
flatten [] = []
flatten (x:xs) = 
  x ++ x2
  where x2 = flatten xs


-- nomes senars
justOdds :: [Int] -> [Int]
justOdds [] = []
justOdds (x:xs) 
  | r /= 0 = [x] ++ justOdds xs
  | otherwise = justOdds xs
  where r = mod  x  2

-- nomes parells
justEvens :: [Int] -> [Int]
justEvens [] = []
justEvens (x:xs)
  | r == 0 = [x] ++ justEvens xs
  | otherwise = justEvens xs
  where r = mod x 2
        
-- retorna 2 llistes, una amb els parells i una amb els senars
oddsNevens :: [Int] -> ([Int],[Int])
oddsNevens l = (l1 , l2)
  where l2 = justEvens l
        l1 = justOdds l

isPrime :: Int -> Bool
isPrime 0 = False
isPrime 1 = False
isPrime x = noDivs x (x - 1)

-- noDivs x y es true si x no es divisible x cap nombre dsd y fins a 2
noDivs :: Int -> Int -> Bool
noDivs x 1 = True
noDivs x y =
  if mod x y == 0 then False
  else noDivs x (y - 1)
       


divsList :: Int -> Int -> [Int]
divsList x 1 = []
divsList x y =
  if (mod x y == 0) && isPrime y then 
    l ++ [y]
  else l
  where l = divsList x (y - 1)
        
-- retorna la llista de divisors primers de lentrada
-- amb repeticions
primeDivisors :: Int -> [Int]
primeDivisors 1 = []
primeDivisors x
  | isPrime x = []
  | otherwise = divsList x (x - 1)
                
