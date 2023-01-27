
--q1
remove::Eq a => [a] -> a -> [a]
remove [] _ = []
remove (x:xs) y
  | x /= y   = x : remove xs y
  | otherwise = xs

--q2
threeN:: [Int] -> [Int]
threeN [] = []
threeN n@(x:xs)
  | x `elem` xs = n
  | even x      = threeN (div x 2 : n)
  | otherwise   = threeN (3*x-1 : n)

--q3
prefix::Eq a=>[a]->[a]->Bool
prefix [] [] = True
prefix _ []  = False
prefix [] _  = True
prefix (x:xs) (m:ms)
  | x == m    = prefix xs ms
  | otherwise = False

--q4
sublist::Eq a=>[a]->[a]->Bool
sublist [] [] = True
sublist _ []  = False
sublist [] _  = True
sublist n@(x:xs) (m:ms)
  | x == m    = prefix xs ms
  | otherwise = sublist n ms


--q5
removeAll::Eq a=>[a]->a->[a]
removeAll xs y = [x | x<-xs, x/=y]

--q6
singleton::[a]->[[a]]
singleton xs = [[x] | x<-xs]

--q7
breakList::[(a,[b])]->[(a,b)]
breakList xs = [(x,y) | (x,ys)<-xs, y<-ys]

--q8
minusPairs::(Ord a,Num a)=>[(a,a)]->[a]
minusPairs xs = [y-x | (x,y)<-xs,x<y]

--q9
--Since [1..] is infinite, the program will run forever through the list [1..]