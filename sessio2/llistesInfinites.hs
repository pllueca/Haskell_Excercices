ones :: [Int] 
ones = cycle [1]

nats :: [Int]
nats = iterate (+1) 0

ne 0 = 1
ne x
  |x < 0 = (-1 * x) + 1
  |otherwise = (-1 * x)
  
ints :: [Int]
ints = iterate ne 0