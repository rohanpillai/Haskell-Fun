doubleMe x = x + x

doubleUs x y = 2*x + 2*y

rohan'Pillai = "Hi it's me, Rohan"

doubleSmallNumber x = if x > 100
                        then x
                      else x*2

doubleSmallNumber' x = (if x > 100 then x else x*2) + 1

removeUpperCase st = [ c | c <- st, c `elem` ['A'..'Z']]

lucky :: (Integral a) => a -> String
lucky 7 = "Yeah it's SEVEN"
lucky x = "Oops"

factorial :: (Integral a) => a -> a
factorial 0 = 1
factorial n = n * factorial (n - 1) 

addVectors :: (Num a) => (a, a) -> (a, a) -> (a, a)
addVectors (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

fibonacci :: (Integral a) => a -> a
fibonacci 1 = 1
fibonacci 2 = 2
fibonacci n = fibonacci (n - 1) + fibonacci (n - 2)

length' :: (Num b) => [a] -> b
length' [] = 0
length' (_:xs) = 1 + length' xs

bmiTell :: (RealFloat a) => a -> a -> String
bmiTell weight height
  | bmi <= skinny = "You are underweight"
  | bmi <= 25 = "Normal"
  | bmi <= 30 = "Over-weight"
  | otherwise = "Obese"
  where bmi = weight / height ^ 2
        skinny = 18.5

initials :: String -> String -> String
initials firstname lastname = [f] ++ "." ++ [l] ++ "."
  where (f:_) = firstname
        (l:_) = lastname

circleArea :: (RealFloat a) => a -> a
circleArea x = 
  let pi = 3.14
  in pi * x ^ 2

cylinder :: (RealFloat a) => a -> a -> a
cylinder radius height = 
  let sideArea = 2 * pi * radius * height
      topArea = pi * radius ^ 2
  in sideArea + 2 * topArea
  where pi = 3.14

replicate' :: (Num i, Ord i) => i -> a -> [a]
replicate' 0 x = []
replicate' y x = x:replicate' (y-1) x

take' :: (Num i, Ord i) => i -> [a] -> [a]
take' _ [] = []
take' n _
  | n <= 0 = []
take' n (x:xs) = x:take' (n-1) xs

reverse' :: (Num a) => [a] -> [a]
reverse' [] = []
reverse' [x] = [x]
reverse' (x:xs) = reverse' xs ++ [x]

zip' :: [a] -> [a] => [(a,a)]
zip' _ [] = []
zip' [] _ = []
zip' (a:as) (b:bs) = (a,b):zip' as bs

elem' :: (Eq a) => a -> [a] -> Bool
elem' _ [] = False
elem' a (x:xs)
  | a == x = True
  | otherwise = elem' a xs

quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) =
  let smallerSort = quicksort [ a | a <- xs, a <= x]
      biggerSort = quicksort [ a | a <- xs, a > x]
  in smallerSort ++ [x] ++ biggerSort

fibList :: (Integral a) => a -> a -> [a]
fibList 0 0 = 1:fibList 1 1
fibList x y = (x + y):fibList (x + y) x

lowerList :: (Integral a) => a -> [a]
lowerList 1 = [1]
lowerList n = n:lowerList (n-1)

lastElem :: [a] -> a
lastElem [x] = x
lastElem (_:xs) = lastElem xs

lastButOne :: [a] -> a
lastButOne [] = error "error"
lastButOne [x] = error "error"
lastButOne [x, _] = x
lastButOne (x:xs) = lastButOne xs

elemAt :: [a] -> Int -> a
elemAt x n = x !! n

isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome [] = True
isPalindrome [x] = True
isPalindrome x
  | (head x) == (last x) = isPalindrome (init (tail x))
  | otherwise = False

data NestedList a = Elem a | List [NestedList a]

compress :: (Eq a) => [a] -> [a]
compress [] = []
compress [x] = [x]
compress (x:y:s)
  | x == y = compress (y:s)
  | otherwise = x:compress (y:s)

--pack' :: (Eq a) => [a] -> [[a]]
--pack' [] = []
--pack' (x:ys@(y:_))
--  | x == y = x: pack' ys
--  | otherwise = [y]: pack' ys 

data BookInfo = Book Int String [String]
                deriving (Show)

data BookReview = BookReview BookInfo String

