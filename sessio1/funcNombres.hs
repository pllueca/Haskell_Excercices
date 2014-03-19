absValue :: Int -> Int
absValue x = 
  if x < 0
  then  x - x - x
  else x

power :: Int -> Int -> Int
power x p =
  if p == 0
  then 1
  else x * (power x (p - 1))
       
esPrim :: Int -> Bool
esPrim 0 = False
esPrim 1 = False
esPrim x = noDivs x (x - 1)

-- noDivs x y es true si x no es divisible x cap nombre dsd y fins a 2
noDivs :: Int -> Int -> Bool
noDivs x 1 = True
noDivs x y =
  if mod x y == 0 then False
  else noDivs x (y - 1)
                       
    
slowFib :: Int -> Int
slowFib 1 = 1
slowFib 2 = 1
slowFib x = (slowFib (x - 1)) + (slowFib (x - 2))
                
-- quick fibonacci utilitzant la formula de binnet
-- F_n = (phi^n - (-phi)^-n)/sqrt(5) 
quickFib :: Int -> Int
quickFib n = round $ phi ** fromIntegral n / sq5
  where
    sq5 = sqrt 5 :: Double -- arrel de 5, tipus Double
    phi = (1 + sq5) / 2 --golden ratio
  