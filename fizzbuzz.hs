fb = zipWith (++) (cycle ["", "", "Fizz"]) (cycle ["", "", "", "", "Buzz"])

pick word num = if null word then num else word

fizzbuzz = zipWith pick fb (map show [1..])

main = do
    print $ take 30 fizzbuzz
