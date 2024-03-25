alpha2Position :: [Int] -> Char -> Int
alpha2Position heights ch = elemIndex ch heights

largestHeight :: String -> Int
largestHeight word = maximum $ map alpha2Position word

totalArea :: [Int] -> String -> Int
totalArea h word = length word * largestHeight word

main = do
    print $ totalArea [1, 3, 1, 3, 1, 4, 1, 3, 2, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 7] zaba
