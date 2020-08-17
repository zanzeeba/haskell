module Lists(asc, head1) where 

-- generating a list

asc :: Int -> Int -> [Int]
asc n m
  | m < n = []
  | m == n = [m]
  | m > n = n : asc (n+1) m
  
head1 :: [Int] -> Int
head1 [] = 0
head1 (x:xs) = x

sum1 :: [Int] -> Int
sum1 [] = 0
sum1 (x:xs) = x + sum1 xs

evens1 :: [Int] -> [Int]
evens1 [] = []
evens1 (x:xs)
  | mod x 2 == 0 = x : evens1 xs
  | otherwise = evens1 xs
  
odds1 :: [Int] -> [Int]
odds1 [] = []
odds1 (x:xs)
  | mod x 2 /= 0 = x : odds1 xs
  | otherwise = odds1 xs
 
-- tuples
-- already defined so i don't need to 
-- do that
fst (x,_, _) = x
snd (_, y, _) = y

-- add tuples
-- essentially adding a list of tuples (pairs)

addTuples :: [(Int, Int)] -> [Int]
addTuples xs = [x+y | (x,y) <- xs ]

add3Tuples :: [(Int, Int, Int)] -> [Int]
add3Tuples xs = [x+y+z | (x,y,z) <- xs ]

-- exercises on lists
--elem :: (Eq a) => a -> [a] Bool

-- my attempt
--elem e (x:xs)
--  | x == 2 = True
--  | otherwise = Lists.elem xs

-- correct answer
elem _ [] = False
elem e (x:xs) = (e == x) || (Lists.elem e xs) 

-- exercise 2 
-- remove all duplicates from a given list
-- mine
--nub1 :: (Eq a) => [a] -> [a]
--nub1 [] = []
--nub1 (x:xs) = x : nub1 xs
--nub1 (x:xs) = x : nub1 xs
--solution
-- what i did not know was the List.Elem existed
-- so x Elem xs does the work for you
nub1 :: (Eq a) => [a] -> [a]
nub1 [] = []
nub1 (x:xs)
  | x `Lists.elem` xs = nub1 xs
  | otherwise   = x : nub1 xs

-- exercise 3
{- make sure a list is in ascending order
al = [1,2,3,4,5,6,7,8,9] True
al = [1,2,2,2,2,6,7,8,9] True
al = [1,2,3,4,5,4,3,2,9] False
-}

isAsc :: [Int] -> Bool
isAsc []  = True
isAsc [x] = True
isAsc (x:y:xs) = x <= y && isAsc (y:xs)
  
-- exercise #4
-- directed graph
-- [(1,2),(2,3),(3,2),(4,3),(4,5)]
-- the nodes will be 1, 2, 3, 4, 5
{-
1 -> 2 True
1 -> 3 True
1 -> 4 False
1 -> 5 False
2 -> 3 True
2 -> 4 False
2 -> 5 False
3 -> 4 True
3 -> 5 False
4 -> 5 True

-}

hasPath :: [(Int,Int)] -> Int -> Int -> Bool
hasPath  [] x y = x == y
hasPath  xs x y
  | x == y = True
  | otherwise =
    let xs1 = [(n,m) | (n,m) <- xs, n /= x ] in 
    or [hasPath xs1 m y | (n,m) <- xs, n == x ]



