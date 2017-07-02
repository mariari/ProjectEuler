import Data.Digits

isPalindrome xs
  | even (length xs) = first == reverse second
  | otherwise        = first == reverse (tail second)
  where (first, second) = splitAt (length xs `div` 2) xs

palindromeRange :: [Integer]
palindromeRange = [x * y| x <- range, y <- range, isPalindrome $ digits 10 (x*y)]
  where range = [100..999]

main :: IO Integer
main = return $ maximum palindromeRange
