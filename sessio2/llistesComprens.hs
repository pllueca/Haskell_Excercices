myMap :: (a -> b) -> [a] -> [b]
myMap f l = [ x | a <- l, let x = (f a)]

myFilter :: (a -> Bool) -> [a] -> [a]
myFilter f l = [x | x <- l, (f x) == True]

-- zip l1 l2 ajunta dues llistes en una llista de parells
myZipWith :: (a-> b -> c) -> [a] -> [b] -> [c]
myZipWith f l1 l2 = [f a b |(a,b) <- zip l1 l2]


thingify :: [Int] -> [Int] -> [(Int, Int)]
thingify l1 l2 = [(a,b) | a <- l1, b <- l2, (mod a b) == 0 ]

factors :: Int -> [Int]
factors x = [y | y <- [1..x], mod x y == 0]
