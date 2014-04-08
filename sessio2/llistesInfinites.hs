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


nt 0 = 1
nt x = 1 + x + nt (x - 1)

triangulars :: [Int]
triangulars = map nt nats


factorial 0 = 1
factorial 1 = 1
factorial n = n * factorial (n - 1)

factorials :: [Int]
factorials = map factorial nats


quickFib :: Int -> Int
quickFib n = round  (phi ** fromIntegral n / sq5)
  where
    sq5 = sqrt 5 :: Double -- arrel de 5, tipus Double
    phi = (1 + sq5) / 2 --golden ratio
  
fibs :: [Int]
fibs = map quickFib nats

isPrime :: Int -> Bool
isPrime 0 = False
isPrime 1 = False
isPrime x = noDivs x (x - 1)

-- noDivs x y es true si x no es divisible x cap nombre dsd y fins a 2
noDivs :: Int -> Int -> Bool
noDivs x 1 = True
noDivs x y =
  if mod x y == 0 then False
  else noDivs x (y - 1)

primes :: [Int]
primes = filter isPrime nats


hamming :: Int -> Bool
hamming x = 

-- nomes tenen 2, 3 i 5 com a divisors primers
hammings :: [Int]
hammings = filter hamming nats

lns x = 

lookNsay :: [Int]
lookNsay = iterate lns [1]
