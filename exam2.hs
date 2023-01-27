
--q1
maxpair xs = map (\(a,b) -> max a b)  xs

--q2
between x y zs = filter (\a -> x <= a && a <= y) zs

--q3
negsquare xs = map (^2) $ filter (<0) xs

--q4
funcpair f g = (\(x,y) -> (f x, g y))

--q5
posneg x y = (\a -> if a>0 then x else y)

--Person
data Person = P String String Char Int
  deriving Show

--q6
diffages (P f l g a) xs = map (P f l g) xs

--q7
instance Eq Person where
  (P f1 l1 g1 a1) == (P f2 l2 g2 a2) = f1 == f2 && l1 == l2

--Tree
data Tree a = Node a [Tree a]
  deriving Show

--q8
count x (Node a t)
  | a == x = 1 + sum (map (count x) t)
  | otherwise = sum (map (count x) t)

--q9
numchildren (Node a t) = Node (length t) $ map numchildren t

--q10
functree f (Node a t) = Node (f a) $ map (functree f) t