import Data.Char
import Data.List

num2Digits :: Int -> [Int]
num2Digits = map digitToInt . show

digits2Num :: [Int] -> Int
digits2Num = foldl1 (\x y -> 10 * x + y)

smallest :: Int -> Int
smallest = digits2Num . sort . num2Digits

largest :: Int -> Int
largest = digits2Num . reverse . sort . num2Digits

next :: Int -> Int
next n = largest n - smallest n

dropUntilRepeat :: Eq a => [a] -> [a]
dropUntilRepeat [] = []
dropUntilRepeat [n] = [n]
dropUntilRepeat (n:ns) = if n == head ns then [n] else n : (dropUntilRepeat ns)

dur :: Eq a => [a] -> [a]
dur xs
      | length xs <= 1            = xs
      | head xs == head (tail xs) = [head xs]
      | otherwise                 = head xs : (dur $ tail xs)

kaprekarSequence :: Int -> [Int]
kaprekarSequence = dropUntilRepeat . iterate next

main = do
  start <- getLine
  print $ kaprekarSequence (read start :: Int)   
