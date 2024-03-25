import Data.List

isPrime :: Int -> Bool
isPrime 1 = False
isPrime 2 = True
isPrime n = (elem 0 [mod n x| x <- [2..(n-1)]]) == False

main = do
print $ filter isPrime [1..20]
