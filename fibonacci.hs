fibs = 0 : 1 : zipWith (+) fibs (tail fibs)

main = do
    print $ take 10 fibs
