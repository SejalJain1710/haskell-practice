nextCollatz :: Int -> Int
nextCollatz n | even n = quot n 2
              | odd n  = 3 * n + 1

collatz :: Int -> [Int]
collatz 4 = [4, 2, 1]
collatz n = n : (collatz $ nextCollatz n)

main = do
    print $ collatz 1
    print $ collatz 7
