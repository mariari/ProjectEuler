
multiples :: Integer -> [Integer]
multiples range = filter threeOrFive [1..range]
  where threeOrFive x = 0 == x `mod` 3 || 0 == x `mod` 5

main :: IO Integer
main = return . sum . multiples $ 999
