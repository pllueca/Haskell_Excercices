--module Queue where

data Queue a = Queue [a][a]
             deriving (Show)

create :: Queue a
create = Queue [] []

push :: a -> Queue a -> Queue a
push x (Queue l1 l2) = (Queue l1 (x:l2))
        
pop :: Queue a -> Queue a
pop (Queue [] l2) = (Queue (drop 1 l1) [])
  where l1 = myReverse l2
pop (Queue l1 l2) = (Queue (drop 1 l1) l2)

empty :: Queue a -> Bool
empty (Queue [] []) = True
empty (Queue l1 l2) = False

top :: Queue a -> a
top (Queue [] l2) = last l2
top (Queue (x:l1) l2) = x
           

instance (Eq a) => Eq (Queue a) where
  Queue [][] == Queue [][] = True
  q1 == q2  
    | (empty q1 || empty q2) = False
    | otherwise = if (top q1) == (top q2) 
             then ((pop q1) == (pop q2)) 
             else False

    
-- aux function        
myReverse :: [a] -> [a]
myReverse = foldl (\l x -> ([x] ++ l)) []
        
sizeQ :: Queue a -> Int
sizeQ (Queue l1 l2) = (length l1) +  (length l2)