myFoldl f x [] = x
myFoldl f x (y:ys) = myFoldl f (f x y) ys


myFoldr :: (a -> b -> b) -> b -> [a] -> b
myFoldr f x [] = x
myFoldr f x (y:ys) = f y $ myFoldr f x ys

myIterate :: (a -> a) -> a -> [a]
myIterate f x = (x:ls)
  where ls = myIterate f (f x)
                      
myUntil :: (a -> Bool) -> (a -> a) -> a -> a
myUntil fb fa x = if fb x then fa myUntil fb fa (fa x)
