insert :: [Int] -> Int -> [Int]
insert [] x = [x]
insert (x:(n:xs)) y 
  | (x < y) && (y < n) = [x,y,n] ++ xs
  | otherwise = [x] ++ insert xs y
