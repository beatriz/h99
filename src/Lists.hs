module Lists where

last' :: [a] -> a
last' (x:[]) = x
last' (x:xs) = last' xs

penultimate :: [a] -> a
penultimate (x : _ : []) = x
penultimate (x : xs) = penultimate xs

elementAt :: [a] -> Int -> a
elementAt ls n
  | n == 1 = head ls
  | otherwise = elementAt (tail ls) (n-1)

myLength :: [a] -> Int
myLength [] = 0
myLength (_ : xs) = (myLength xs) + 1

myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x : xs) = myReverse xs ++ [x]
