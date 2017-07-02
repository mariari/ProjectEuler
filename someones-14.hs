import Data.Function.Memoize (memoFix)
import Data.List (maximumBy)
import Data.Ord (comparing)


-- all if thens changed to | 

cl :: (Integer -> Integer) -> Integer -> Integer
cl _ 1 = 1
cl f n
  | even n    = 1 + f (n `div` 2)
  | otherwise = 1 + f (3 * n + 1)

cl' :: Integer -> Integer
cl' = memoFix cl

-- my edit, it's slower in the repl by far, and fater compiled! and takes up less memory
cl'' ::  Integer -> Integer
cl'' 1 = 1
cl'' n
  | even n    = 1 + cl'' (n `div` 2)
  | otherwise = 1 + cl'' (3 * n + 1)


main :: IO ()
main = print . fst . maximumBy (comparing snd) . zip nums $ cl' <$> nums
  where nums = [1..1000000]
