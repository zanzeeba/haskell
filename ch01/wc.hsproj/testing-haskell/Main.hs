-- file: ch01/WC.hs
-- lines beginning with "--" are comments.

main = interact wordCount
    where wordCount input = show (length (lines input)) ++ "\n"


wiffle x = x + 5

lucky :: Int -> String
lucky 7 = "LUCKY NUMBER SEVEN!"
lucky x = "Sorry, you're out of luck, pal!"

sayMe :: Int -> String
sayMe 1 = "One!"
sayMe 2 = "Two!"
sayMe 3 = "Three!"
sayMe 4 = "Four!"
sayMe 5 = "Five!"
sayMe x = "Not between 1 and 5"

--fizz :: Int -> String
--fizz n | n `mod` 15 == 0  = "FizzBuzz"
--       | n `mod` 3  == 0  = "Fizz"
--       | n `mod` 5  == 0  = "Buzz"
--       | otherwise        = show n

--mainx :: IO()
--mainx = mapM_ putStrLn $ map fizz [1..100]

charName :: Char -> String
charName 'a' = "Albert"
charName 'b' = "Broseph"
charName 'c' = "Cecil"
charName 'd' = "Dave"
charName 'e' = "Edward"
charName 'f' = "Frederick"
charName 'g' = "George"
charName 'h' = "Henry"
charName 'i' = "Ian"
charName x = "Oopsy Bummer"

addVectors :: (Double, Double) -> (Double, Double) -> (Double, Double)
addVectors a b = (fst a + fst b, snd a + snd b)

head' :: [a] -> a
head' [] = error "Can't call head on an empty list, dummy!"
head' (x:_) = x

xs = [(1,3),(4,3),(2,4),(5,3),(5,6),(3,1)]
ww = [a+b | (a, b) <- xs]


tell :: (Show a) => [a] -> String
tell [] = "The list is empty"
tell (x:[]) = "The list has one element: " ++ show x
tell (x:y:[]) = "The list has two elements: " ++ show x ++ " and " ++ show y
tell (x:y:_) = "This list is long. The first two elements are: " ++ show x
               ++ " and " ++ show y
               
squares = [x * x | x <- [1..100]]


bmiTell :: Double -> String
bmiTell bm
    | bm <= 18.5 = "You're underweight, eat more!"
    | bm <= 25.0 = "Looking good!"
    | bm <= 30.0 = "You're overweight. Let's work out together!"
    | otherwise   = "You're obese. Go see a doctor."
    


bmiTellz :: Double -> Double -> String
bmiTellz weight height
    | bmi <= skinny = "You're underweight, eat more!"
    | bmi <= normal = "Looking good!"
    | bmi <= fat    = "You're overweight. Let's work out together!"
    | otherwise     = "You're obese. Go see a doctor."
    where bmi = weight / height ^ 2
          skinny = 18.5
          normal = 25.0
          fat = 30.0
          

headx :: [a] -> a
headx [] = error "no head for empty lists buddy!"
headx (x:_) = x

heady :: [a] -> a
heady xs = case xs of [] -> error "No head for empty lists buddy 2!"   

describeList :: [a] -> String
describeList ls = "The list is " ++ what ls
    where what [] = "empty."
          what [x] = "a singleton list."
          what xs = "a longer list."

 -- use describeList[]


maximumb :: (Ord a) => [a] -> a
maximumb [] = error "maximum of empty list!"
maximumb [x] = x
maximumb (x:xs) = max x (maximumb xs)


quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) =
    let smallerOrEqual = [a | a <- xs, a <= x]
        larger = [a | a <- xs, a > x]
    in  quicksort smallerOrEqual ++ [x] ++ quicksort larger
    

-- primes = filterPrime [2..]
--  where filterPrime (p:xs) = p : filterPrime [x | x <- xs,x `mod` p /= 0]


sumx :: (Num a) => [a] -> a
sumx xs = foldl (\acc x -> acc + x) 0 xs

sumy :: (Num a) => [a] -> a
sumy = foldl (+) 0


data BookInfo = Book Int String [String] deriving (Show)


data Spin = Up | Down





n = a `div` length xs
    where
      a = 10
      xs = [1,2,3,4,5]

evenx :: Integral a => a -> Bool
evenx n = n `mod` 2 == 0

splitAtx :: Int -> [a] -> ([a],[a])
splitAtx n xs = (take n xs, drop n xs)

absx :: Int -> Int
absx n = if n >= 0 then n else -n

signumx :: Int -> Int
signumx n = if n < 0 then -1 else
  if n == 0 then 0 else 1

signumy n | n < 0 = -1
          | n == 0 = 0
          | otherwise =1

fstx :: (a,b) -> a
fstx (x,_) = x

odds :: Int -> [Int]
odds n = map (\x -> x*2 + 1 ) [0..n-1]

concatx xss = [x | xs <- xss, x <- xs]

factors n = [x | x <- [1..n], n `mod` x == 0]

prime  n = factors n == [1,n]
primes n = [x | x <- [2..n], prime x]


fizzbuzz = [fb x| x <- [1..100]]
    where fb y
            | y `mod` 15 == 0 = "FizzBuzz"
            | y `mod` 3  == 0 = "Fizz"
            | y `mod` 5  == 0 = "Buzz"
            | otherwise  = show y

-- first in_range with types defined
in_range :: Integer -> Integer -> Integer -> Bool
in_range min max x = x >= min && x <= max

-- in_range with returns but not using return
in_range_a min max x = 
  let in_lower_bound = min <= x
      in_upper_bound = max >= x
  in
  in_lower_bound && in_upper_bound
  





