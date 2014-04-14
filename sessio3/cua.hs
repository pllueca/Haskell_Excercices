--module Queue where

data Queue a = Queue [a][a]
             deriving (Show)

create :: Queue a
create = Queue [] []

push :: a -> Queue a -> Queue a
push x (Queue l1 l2) = (Queue l1 (x:l2))
        
pop :: Queue a -> Queue a
pop (Queue [] l2) = (Queue (drop 1 l1) [])
  where l1 = reverseList l2
pop (Queue l1 l2) = (Queue (drop 1 l1) l2)

empty :: Queue a -> Bool
empty (Queue [] []) = True
empty (Queue l1 l2) = False

top :: Queue a -> a
top (Queue [] l2) = last l2
top (Queue (x:l1) l2) = x
           

reverseList :: [a] -> [a]
reverseList [] = []
reverseList [x] = [x]
reverseList (x:xs) = l ++ [x]
  where l = reverseList xs
        
instance (Eq a) => Eq (Queue a) where
  (Queue [][]) == (Queue [][]) = True
  q1 == q2 = ((top q1) == (top q1)) && (pop q1 == pop q2)
  