import Data.List

doubleMe x = x + x

doubleUs x y = x*2 + y*2

doubleSmallNumber x = if x > 100
	then x
	else x*2
	
lostNumber = [4,8,15,16,23,42]

lucky :: Int -> String
lucky 7 = "СЧАСТЛИВОЕ ЧИСЛО 7!"
lucky x = "Прости друг, повезет в другой раз!"


removeNonUppercase :: [Char] -> [Char]
removeNonUppercase st = [c | c <- st, c `elem` ['A'..'Z']]

addThree :: Int -> Int -> Int -> Int
addThree x y z = x + y + z

factorial' :: Integer -> Integer
factorial' n = product [1..n]

circumference :: Float -> Float
circumference r = 2 * pi * r

circumference' :: Double -> Double
circumference' r = 2 * pi * r

sayMe :: Int -> String
sayMe 1 = "One!"
sayMe 2 = "Two!"
sayMe 3 = "Three!"
sayMe 4 = "Four!"
sayMe 5 = "Five!"
sayMe _ = "This number is not value 1-5"

factorial :: Integer -> Integer
factorial 0 = 1
factorial n = n * factorial (n - 1)

charName :: Char -> String
charName 'a' = "Artem"
charName 'b' = "Boris"
charName 'v' = "Victor"

addVectors' :: (Double, Double) -> (Double, Double) -> (Double, Double)
addVectors' a b = (fst a + fst b, snd a + snd b)

addVectors :: (Double, Double) -> (Double, Double) -> (Double, Double)
addVectors (x1, y1) (x2, y2) = (x1+ x2, y1 + y2)

first :: (a,b,c) -> a
first (x, _, _) = x

second :: (a,b,c) -> b
second (_, y, _) = y

third :: (a,b,c) -> c
third (_, _, z) = z

head' :: [a] -> a 
head' [] = error "Empty list"
head' (x:_) = x

tell :: (Show a) => [a] -> String
tell [] = "Empty list"
tell (x:[]) = "One element: " ++ show x
tell (x:y:[]) = "Two elements: " ++ show x ++ " and " ++ show y
tell (x:y:_) = "To long. First two elements: " ++ show x ++ " and " ++ show y 

badAdd :: (Num a) => [a] -> a
badAdd (x:y:z:[]) = x + y + z

firstLetter :: String -> String
firstLetter " " = "Empty string"
firstLetter all@(x:xs) = "First letter " ++ all ++ " is " ++ [x]

abc :: (a, a) -> (a, a)
abc all@(a,b) = all

bmiTell' :: Double -> String
bmiTell' bmi
 | bmi <= 18.5 = "Emo"
 | bmi <= 25.0 = "Norma"
 | bmi <= 30.0 = "Fat"
 | otherwise = "Very fat"
 

 
max' :: (Ord a) => a -> a -> a
max' a b
 | a <= b = b
 | otherwise = a
 
myCompare :: (Ord a) => a -> a -> Ordering
a `myCompare` b
 | a == b = EQ
 | a <= b = LT
 | otherwise = GT

bmiTell :: Double -> Double -> String
bmiTell weight height 
 | bmi <= skinny = "Emo"
 | bmi <= normal = "Norma"
 | bmi <= fat = "Fat"
 | otherwise = "Very fat"
 where bmi = weight / height ^ 2
       (skinny, fat, normal) = (18.5, 25.0, 30.0)

niceGreeting :: String
niceGreeting = "Hello"

badGreeting :: String
badGreeting = "Hi"	
	  
greet :: String -> String
greet "Huan" = niceGreeting ++ " Huan!"
greet "Fernando" = niceGreeting ++ " Fernando!"
greet name = badGreeting ++ " " ++ name

initials :: String -> String -> String
initials firstname lastname = [f] ++ ", " ++ [l] ++ "."
 where (f:_) = firstname 
       (l:_) = lastname	
	   
calcBmis :: [(Double, Double)] -> [Double]
calcBmis xs = [bmi w h | (w, h) <- xs]
 where bmi weight height = weight / height ^ 2
 
cylinder :: Double -> Double -> Double
cylinder r h = 
 let sideArea = 2 * pi * r * h
     topArea = pi * r ^ 2
 in sideArea + 2 * topArea

trues :: Integer -> Integer -> Bool
trues x y 
 | x^2 == y = True
 | otherwise  = False
 
truess x y = x^2 == y 

truess' = truess 2

currys x = (\y z -> x + y + z)
currys' x = currys x

qsort :: Ord a => [a] -> [a]
qsort [] = []
qsort (x:xs) = n (<=) ++ [x] ++ n (>)
 where n y = qsort [a | a <- xs, a `y` x]
 


qsort' :: Ord a => [a] -> [a]
qsort' [] = []
qsort' (x:xs) =
 let n y = qsort [a | a <- xs, a `y` x]
 in n (<=) ++ [x] ++ n (>)
 
square x = x^2

fourth = square . square

calcBmis' :: [(Double, Double)] -> [Double]
calcBmis' xs = [bmi | (w,h) <- xs, let bmi = w / h ^ 2, bmi > 25.0]

head'' :: [a] -> a
head'' xs =
 case xs of
  [] -> error "123"
  (x:_) -> x
 
describeList :: [a] -> String
describeList xs = "List" ++
 case xs of
  [] -> "Empty"
  [x] -> "One"
  xs -> "Long"
  
describeList' :: [a] -> String
describeList' xs = "List" ++ what xs
 where
  what [] = "Empty"
  what [x] = "One"
  what xs = "Long"
  
maximum' :: (Ord a) => [a] -> a
maximum' [] = error "[]"
maximum' [x] = x
maximum' (x:xs) = max x (maximum' xs)

replicate' :: Int -> a -> [a]
replicate' n x
 | n <= 0 = []
 | otherwise = x : replicate (n-1) x
 
take' :: (Num i, Ord i) => i -> [a] -> [a]
take' n _
 | n <= 0 = []
take' _ [] = []
take' n (x:xs) = x : take' (n-1) xs

reverse' :: [a] -> [a]
reverse' [] = []
reverse' (x:xs) = reverse' xs ++ [x]
 
repeat' :: a -> [a]
repeat' x = x : repeat' x

zip' :: [a] -> [b] -> [(a,b)]
zip' _ [] = []
zip' [] _ = []
zip' (x:xs) (y:ys) = (x,y):zip' xs ys

elem' :: (Eq a) => a -> [a] -> Bool
elem' a [] = False
elem' a (x:xs)
 | a == x = True
 | otherwise = a `elem'` xs
 
quicksort :: (Ord a) => [a] -> [a]
quicksort (x:xs) =
 let smallerSorted = quicksort [a | a <- xs, a <= x]
     biggerSorted = quicksort [a | a <- xs, a > x]
 in smallerSorted ++ [x] ++ biggerSorted
quicksort [] = []

multiThree :: Int -> (Int -> (Int -> Int))
multiThree x y z = x * y * z

compareWithHundred :: Int -> Ordering
compareWithHundred x = compare 100 x

divideByTen :: (Floating a) => a -> a
divideByTen = (/10)

isUpperAlphanum :: Char -> Bool
isUpperAlphanum = (`elem` ['A'..'Z'])

minus :: Int -> Int 
minus = (-) 9

applyTwice :: (a -> a) -> a -> a
applyTwice f x = f (f x)

zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ [] _ = []
zipWith' _ _ [] = []
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys

flip' :: (a -> b -> c) -> (b -> a -> c)
flip' f = g
 where g x y = f y x
 
map' :: (a -> b) -> [a] -> [b]
map' _ [] = []
map' f (x:xs) = f x : map' f xs

filter' :: (a -> Bool) -> [a] -> [a]
filter' _ [] = []
filter' p (x:xs)
 | p x = x : filter' p xs
 | otherwise = filter' p xs
 
quicksort' :: (Ord a) => [a] -> [a]
quicksort' [] = []
quicksort' (x:xs) =
 let smallerSorted = quicksort' (filter (<= x) xs)
     biggerSorted = quicksort' (filter (> x) xs)
 in smallerSorted ++ [x] ++ biggerSorted
 
largestDivisible :: Integer
largestDivisible = head (filter p [100000, 99999..])
 where p x = x `mod` 3829 == 0
 
chain :: Integer -> [Integer]
chain 1 = [1]
chain n
 | even n = n:chain (n `div` 2)
 | odd n  = n:chain (n*3 + 1)
 
numLongChains :: Int
numLongChains = length (filter isLong (map chain [1..100]))
 where isLong xs = length xs > 15
 
numLongChains' :: Int
numLongChains' = length (filter (\xs -> length xs > 15) (map chain [1..100]))

addThree'' :: Int -> Int -> Int -> Int
addThree'' x y z = x + y + z

addThree' :: Int -> Int -> Int -> Int
addThree' = \x -> \y -> \z -> x + y + z

flip'' :: (a -> b -> c) -> b -> a -> c
flip'' f = \x y -> f y x

sum' :: (Num a) => [a] -> a
sum' xs = foldl (\acc x -> acc + x) 0 xs

sum'' :: (Num a) => [a] -> a
sum'' = foldl (+) 0

elem'' :: (Eq a) => a -> [a] -> Bool
elem'' y ys = foldl (\acc x -> if x == y then True else acc) False ys

map'' :: (a -> b) -> [a] -> [b]
map'' f xs = foldr (\x acc -> f x : acc) [] xs

maximum'' :: (Ord a) => [a] -> a
maximum'' = foldl1 max

reverse'' :: [a] -> [a]
reverse'' = foldl (\acc x -> x : acc) []

reverse''' :: [a] -> [a]
reverse''' = foldl (flip (:)) []

product' :: (Num a) => [a] -> a
product' = foldl (*) 1

filter'' :: (b -> Bool) -> [b] -> [b]
filter'' p = foldr (\x acc -> if p x then x : acc else acc) []

{-last' :: [a] -> a
last' = foldr1 (\acc -> acc) -}

and' :: [Bool] -> Bool
and' xs = foldr (&&) True xs

sqrtSums :: Int
sqrtSums = length (takeWhile (< 1000) (scanl1 (+) (map sqrt [1..]))) +1

fn x = ceiling (negate (tan (cos (max 50 x))))

fn' = ceiling . negate . tan . cos . max 50

oddSquareSum :: Integer
oddSquareSum = sum (takeWhile (<10000) (filter odd (map (^2) [1..])))

oddSquareSum' :: Integer
oddSquareSum' = sum . takeWhile (<10000) . filter odd $ map (^2) [1..]

numUniques :: (Eq a) => [a] -> Int
numUniques = length . nub

wordNums :: String -> [(String, Int)]
wordNums = map (\ws -> (head ws, length ws)) . group . sort . words

