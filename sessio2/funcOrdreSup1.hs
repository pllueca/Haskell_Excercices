power :: Int -> Int -> Int
power x p =
  if p == 0
  then 1
  else x * (power x (p - 1))


compt a b = a == b

noF :: [Bool] -> Bool
noF [] = True
noF (x:xs) = (x /= False) && noF xs

-- zipWith aplica compt a les 2 llistes
eql :: [Int] -> [Int] -> Bool
eql l1 l2 = noF l3
  where l3 = zipWith compt l1 l2
                 
             
funcMult :: [Int] -> Int
funcMult [] = 1
funcMult (x:xs) = x * funcMult xs
--prod :: [Int] -> Int


powersOf2 :: [Int]
powersOf2 = [i| j <- [1..], i <- [power 2 j]]

             


