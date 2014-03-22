compt a b = a == b

noF :: [Bool] -> Bool
noF [] = True
noF (x:xs) = (x /= False) && noF xs

-- zipWith aplica compt a les 2 llistes
eql :: [Int] -> [Int] -> Bool
eql l1 l2 = noF l3
  where l3 = zipWith compt l1 l2
                 
             
