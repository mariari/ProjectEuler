fibNums :: [Integer]
fibNums = 1 : 2 : zipWith (+) fibNums (tail fibNums)

evenFib :: [Integer]
evenFib = filter even fibNums

evenFibBelowX :: Integer -> [Integer]
evenFibBelowX x = takeWhile (<= x) evenFib

main :: IO Integer
main = return . sum . evenFibBelowX $ 4000000
