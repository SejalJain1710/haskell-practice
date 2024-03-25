num2Digits :: Int -> [Int]
num2Digits n = if n < 10 then [n] else num2Digits (quot n 10) ++ [mod n 10]

main = do
    print $ num2Digits 2341
