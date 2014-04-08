myFoldl f x [] = x
myFoldl f x (y:ys) = myFoldl f (f x y) ys


myFoldr f x [] = x
myFoldr f x (y:ys) = f y $ myFoldr f x ys

myIterate :: (a -> a) -> a -> [a]
myIterate f x = (x:ls)
  where ls = myIterate f (f x)
                      
myUntil :: (a -> Bool) -> (a -> a) -> a -> a
myUntil fb fa x = if fb x then x
                  else myUntil fb fa $ fa x
                       
myMap :: (a -> b) -> [a] -> [b]
myMap f [] = []
myMap f (x:xs) = [f x] ++ myMap f xs

myFilter :: (a -> Bool) -> [a] -> [a]
myFilter f [] = []
myFilter f (x:xs) = if f x then [x] ++ myFilter f xs
                    else  myFilter f xs
                          
myAll :: (a -> Bool) -> [a] -> Bool
myAll f [] = True
myAll f (x:xs) = if f x then myAll f xs
                 else False
                      
myAny :: (a -> Bool) -> [a] -> Bool
myAny f [] = False
myAny f (x:xs) = if f x then True
                 else myAny f xs

myZip :: [a] -> [b] -> [(a, b)]
myZip [] y = []
myZip x [] = []
myZip (x:xs) (y:ys) = [(x,y)] ++ myZip xs ys

myZipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
myZipWith f [] y = []
myZipWith f x [] = []
myZipWith f (x:xs) (y:ys) = [f x y] ++ myZipWith f xs ys
