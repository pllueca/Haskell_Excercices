-- Problema 1
genPairs :: [Int] -> [Int] -> [Int] -> [(Int,Int)]
genPairs l1 l2 l3 = [(x,y) | x <- l1, y <- l2, 
                     ((elem x l3) && (not $ elem y l3) 
                     || (not $ elem x l3) && (elem y l3))]
                    
-- Problema 2


-- Problema 3
data Arbre a = Node a (Arbre a) (Arbre a) | Abuit 
             deriving (Show)
               
-- 3.2
ttake :: Arbre a -> Int -> Arbre a
ttake (Node x f1 f2) c 
  | c == 0 = Abuit
  | otherwise = (Node x f1' f2')
  where 
    f1' = ttake f1 (c - 1)
    f2' = ttake f2 (c - 1)
        
-- 3.3 
inftree :: Arbre Int
inftree = inftreep 1
          where 
            inftreep x = (Node x (inftreep (x+1)) (inftreep (x+1)))
            
-- Problema 4
data ErrorList a = Llista [a] Int

instance (Eq a) => Eq (ErrorList a) where
  (Llista l1 e1) == (Llista l2 e2) = ((difs <=  e1) && (difs <= e2))
                                     where 
                                       difs =  (abs $ (length l1) - (length l2)) + foldl (\acc x -> if x == True then acc + 1 else acc) 0 (zipWith (/=) l1 l2)