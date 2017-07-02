import Data.Ord
import Data.List
import Data.MemoTrie         as MT

data Collatz = Collatz { val :: Integer
                       , lis :: [Integer]
                       , len :: Int
                       } deriving (Show)

collatz :: Integer -> Collatz
collatz x = f x (Collatz x [] 0)
  where
    f 1 (Collatz val acc len) = Collatz val (1:acc) (1 + len)
    f x (Collatz val acc len)
      | even x     = f (x `div` 2) (Collatz val (x:acc) (1 + len))
      | otherwise  = f (3 * x + 1) (Collatz val (x:acc) (1 + len))


coll :: (Integer -> Collatz -> Collatz) -> Integer -> Collatz -> Collatz
coll f 1 (Collatz val acc len) = Collatz val (1:acc) (1 + len)
coll f x (Collatz val acc len)
  | even x     = f (x `div` 2) (Collatz val (x:acc) (1 + len))
  | otherwise  = f (3 * x + 1) (Collatz val (x:acc) (1 + len))


collMem = MT.memoFix coll

collMem' :: Integer  -> Collatz
collMem' x = (`collMem` (Collatz x [] 0)) x

maxCollatz :: Integer -> Collatz
maxCollatz x = maximumBy (comparing len) $ collMem' <$> [1,3..x]


main :: IO ()
main = print $ maxCollatz 999999
