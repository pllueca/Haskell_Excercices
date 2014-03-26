--power :: Int -> Int -> Int
power x p =
  if p == 0 then 
    1
  else 
    x * (power x (p - 1))

eqBool :: [Bool] -> Bool
eqBool l = foldl (&&) True l

-- zipWith aplica compt a les 2 llistes
eql :: [Int] -> [Int] -> Bool
eql l1 l2 = eqBool l3 && (length l1 == length l2)
  where l3 = zipWith (==) l1 l2
               
powersOf2 = iterate (* 2)  1

powersOf x = iterate  (* x) 1


-- foldl aplica el primer argument a la llista i acumula el
-- resusltat al segon argument
prod :: [Int] -> Int
prod l = foldl (*) 1 l

prodOfEvens = foldl (\acc x -> if (mod x 2 == 0) then acc * x else acc) 1
         
cont0  = foldl (\acc x -> if x == 0 then acc+1 else acc) 0


scalarProduct :: [Float] -> [Float] -> Float
scalarProduct l1 l2 = foldl (+) 0 $ zipWith (*) l1 l2
