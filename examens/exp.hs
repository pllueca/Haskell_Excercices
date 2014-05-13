data Expressio a = Unari (a -> a) (Expressio a) | 
                   Binari (a -> a -> a) (Expressio a) (Expressio a) | 
                   Fulla a
                   
aval (Unari f x) = f (aval x)
aval (Binari f x1 x2) = f (aval x1) (aval x2)
aval (Fulla x) = x

dc trivial resol parteix combina problema =
  | trivial problema = resol problema
  | otherwise = combina problema map (dc trivial resol parteix combina) subprob 
  where subprob = parteix problema
        
