module Tree where

data Tree a = Node a (Tree a) (Tree a) | Empty deriving (Show)

abuit = Empty

plantar x a1 a2 = Node x a1 a2

arrel (Node x _ _) = x

fe (Node _ a1 _) = a1
fd (Node _ _ a2) = a2

isEmpty Empty = True
isEmpty (Node x _ _) = False
