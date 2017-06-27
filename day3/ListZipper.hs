module ListZipper where

data ListPath a = ListPath [a] a [a] deriving Show

goRight :: ListPath a -> ListPath a
goRight (ListPath left a (x:xs)) = ListPath (a:left) x xs
goRight (ListPath left a []) = ListPath left a []

goLeft :: ListPath a -> ListPath a
goLeft (ListPath (x:xs) a right) = ListPath xs x (a:right)
goLeft (ListPath [] a right) = ListPath [] a right

updateCurrent :: (a -> a) -> ListPath a -> ListPath a
updateCurrent f (ListPath left x right) = ListPath left (f x) right

getCurrent :: ListPath a -> a
getCurrent (ListPath _ x _) = x
