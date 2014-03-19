-- exemple ús if then else

prod :: Int -> Int -> Int
prod n m = if n == 0 then 0
       	   else n + prod (n-1) m

-- exemple ús pattern matching

sumar [] = 0
sumar (x:xs) = x+(sumar xs)

-- exemple ús pattern matching més complicat i ús de guardes

select [] = []
select ((x,y):xs) 
       | x>=y = x:(select xs) 
       | otherwise = y:(select xs) 


-- exemple ús pattern matching al where amb resultat tupla

division n m 
    | n < m = (0,n)
    | otherwise = (q+1,r)
    where (q,r) = division (n-m) m
