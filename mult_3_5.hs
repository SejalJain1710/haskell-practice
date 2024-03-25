mult_3_5 :: Int -> Bool
mult_3_5 n = mod n 3 * mod n 5 == 0

main = do
print $ filter mult_3_5 [1..10]
