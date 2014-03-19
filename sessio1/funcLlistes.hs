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
buildPalindrome l = ls ++ l
  where ls = reverseList l

isIn :: Int -> [Int] -> Bool
isIn x [] = False
--isIn x (x:xs) = True
isIn x (y:xs)
  | x == y = True
  | otherwise = (isIn x xs)

removeE :: [Int] -> Int -> [Int]
removeE (x:xs) y 
  | x == y    = removeE xs y
  | otherwise = [x] ++ (removeE xs y)


-- remove x y l
remove :: [Int] -> [Int] -> [Int]
remove l [] = l
remove [] _ = []
remove l (x:xs) = remove (removeE l x) xs

