nextRow r = zipWith (+) ([0] ++ r) (r ++ [0])
pascal = iterate nextRow

main = do
    print $ take 5 (pascal[1])
    print $ pascal[1] !! 0
    print $ pascal[1] !! 3
