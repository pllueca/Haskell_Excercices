import Tree

size :: Tree a -> Int
size t
  | isEmpty t = 0
  | otherwise = 1 + (size (fe t)) + (size (fd t))
                
height :: Tree a -> Int
height t
  | isEmpty t = 0
  | otherwise = 1 + max (height (fe t)) (height (fd t))
                
equal :: Eq a => Tree a -> Tree a -> Bool
equal Empty Empty = True
equal a1 a2 = if (arrel a1) == (arrel a2) then
                (equal (fe a1) (fe a2)) && (equal (fd a1) (fd a2))
              else
                False
                
isomorphic :: Eq a => Tree a -> Tree a -> Bool
isomorphic Empty Empty = True
isomorphic a1 a2 = if (arrel a1) == (arrel a2) then
                (equal (fe a1) (fe a2)) && (equal (fd a1) (fd a2))
                || (equal (fe a1) (fd a2)) && (equal (fd a1) (fe a2))
              else
                False
                
preOrder :: Tree a -> [a]
preOrder Empty = []
preOrder t = [arrel t] ++ preOrder (fe t) ++ preOrder (fd t)

postOrder :: Tree a -> [a]
postOrder Empty = []
postOrder t = postOrder (fe t) ++ postOrder (fd t) ++ [arrel t]

inOrder :: Tree a -> [a]
inOrder Empty = []
inOrder t 
  | (isEmpty (fe t)) && (isEmpty (fd t)) = [arrel t]
  | otherwise = (inOrder (fe t)) ++ [arrel t] ++ (inOrder (fd t))
                
-- breadthFirst :: Tree a -> [a]
-- breadthFirst Empty = []
-- breadthFirst t = 


build :: Eq a => [a] -> [a] -> Tree a


overlap :: (a -> a -> a) -> Tree a -> Tree a -> Tree 
