import qualified Data.Map as Map
import Data.List
import Data.Maybe

under20 = ["", "One ", "Two ", "Three ", "Four ", "Five ", "Six ", "Seven ", "Eight ", "Nine ", "Ten ", "Eleven ", "Twelve ", "Thirteen ", "Fourteen ", "Fifteen ", "Sixteen ", "Seventeen ", "Eighteen ", "Nineteen "]
tens = ["", "Ten ", "Twenty ", "Thirty ", "Forty ", "Fifty ", "Sixty ", "Seventy ", "Eighty ", "Ninety "]

z0 = "Zero "
h100 = "Hundred "
h100and = "Hundred and "

wPeriods = [10 ^ 12, 10 ^ 9, 10 ^ 6, 1000, 1]
wNames = ["Trillion ", "Billion ", "Million ", "Thousand ", ""]
wPeriodsNames = Map.fromDescList $ zip wPeriods wNames

iPeriods = [10 ^ 7, 10 ^ 5, 1000, 1]
iNames = ["Crore ", "Lakh ", "Thousand ", ""]
iPeriodsNames = Map.fromList $ zip iPeriods iNames

convert2Digits :: Int -> String
convert2Digits n
                | n < 20 = under20 !! n
                | otherwise = tens !! (div n 10) ++ under20 !! (mod n 10)

convert3Digits :: Int -> String
convert3Digits n
                | n < 100 = convert2Digits n
                | mod n 100 == 0 = convert2Digits (div n 100) ++ h100
                | otherwise = convert2Digits (div n 100) ++ h100and ++ convert2Digits(mod n 100)

splitPeriods :: Int -> [Int] -> [(Int, Int)]
splitPeriods num periods
                        | length periods == 1 = [(num, head periods)]
                        | otherwise            = (div num p, p) : splitPeriods (mod num p) (tail periods) where p = head periods

convert2Words :: (Int, Int) -> String
convert2Words (0, p) = ""
convert2Words (mult, p)
                    | elem p wPeriods = convert3Digits mult ++ ( fromJust $ Map.lookup p wPeriodsNames )
                    | elem p iPeriods = convert3Digits mult ++ ( fromJust $ Map.lookup p iPeriodsNames )

fig2Words :: Int -> [Int] -> String
fig2Words 0 periods = z0
fig2Words num periods = concat $ Data.List.map convert2Words $ splitPeriods num periods

main = do
    print $ fig2Words 21473647 wPeriods
    print $ fig2Words 0 wPeriods
