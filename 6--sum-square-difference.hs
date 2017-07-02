
triFormula l = l * (l + 1) `div` 2

sumOfSquares :: (Num c, Enum c) => c -> c
sumOfSquares x = sum . fmap (^ 2) $ [1..x]

squareOfSums :: Integer -> Integer
squareOfSums =  (^ 2) . triFormula 

difference :: Integer -> Integer
difference = (-) . squareOfSums <*> sumOfSquares


main :: IO Integer
main = return $ difference 100
