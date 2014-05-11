-- Problema 1
nt 0 = 1
nt x = 1 + x + nt (x - 1)

genSum :: [Int]
genSum = 0 : map nt (iterate (+1) 0)

esSum :: Int -> Bool

          