import Data.Char

digits2Num d = if length d == 1 then head d else (digits2Num (init d)) * 10 + (last d)

f a b = a * 10 + b
digitsToNum d = foldl1 f d

d2n = foldl1 (\x y -> 10 * x + y)
--d2n = foldl1 f

main = do
    print $ digits2Num [1, 2, 3]
    print $ digitsToNum [4, 5, 6]
    print $ d2n [7, 8, 9]
