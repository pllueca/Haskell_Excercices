myFoldl :: (a -> b -> a) -> a -> [b] -> a 
myFoldl f x [] = x  
myFoldl f x (y:ys) = myFoldl f (f y x) ys  

myFoldr :: (a -> b -> b) -> b -> [a] -> b
myFoldr f x [] = x  
myFoldr f x (y:ys) = f y (myFoldr f x ys) 