flatten :: [[Int]] -> [Int]
flatten [] = []
flatten l = foldl (++) [] l

--myLength :: String -> Int
myLength  = foldl (\acc x -> acc+1) 0

--myReverse :: [Int] -> Int
myReverse = foldl (\l x -> ([x] ++ l)) []

-- compta aparicions de n a una llista
countn n = foldl (\acc x -> if x == n then (acc + 1) else acc) 0

-- retorna la llista q indica quantes vegades apareix el x a cada llista de l 
countIn :: [[Int]] -> Int -> [Int]
countIn l x = map (countn x) l
         
firstWord l = takeWhile (/= ' ') (dropWhile (== ' ') l)