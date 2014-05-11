-- Problema 1
sum2 :: [[Int]] -> Int
sum2 l = foldr ((\l2 x -> (foldl (+) 0 l2) + x)) 0 l

-- Problema 2
fsmap :: a -> [(a -> a)] -> a
fsmap x [] = x
fsmap x (f:fs) = fsmap (f x) fs

-- Problema 3
knoDiv :: Int -> [Int] -> Int
knoDiv x l = l1 !! x
             where l1 = [xs | xs <- [0..], nodiv xs l]
                   
nodiv x [] = True
nodiv x (l:ls) 
  | mod x l == 0 = False
  | otherwise = nodiv x ls
                

-- Problema 4
data ArbreGen = Node Int [ArbreGen] | Buit

simetric :: ArbreGen -> ArbreGen
simetric Buit = Buit
simetric (Node x lfills) = (Node x l2)
  where l2 = reverse (map (simetric) lfills)

-- Problema 5 
