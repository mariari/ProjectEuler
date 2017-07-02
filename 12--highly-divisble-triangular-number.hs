import           Data.List.Ordered
import qualified Data.Set          as S
import           Data.Monoid

triFormula :: Integral a => a -> a
triFormula l = l * (l + 1) `div` 2

triNums :: [Integer]
triNums  = triFormula <$> [1..]

triSlice :: Integral t => t -> t -> [t]
triSlice start end = triFormula <$> [start..end]


-- taken from the Haskell wiki
primes :: [Integer]
primes = 2 : 3 : [5,7..] `minus` unionAll [[p*p, p*p+2*p..] | p <- tail primes]

-- gives all prime factors and the number of times each factor goes into each one
primeFactorsRepeat :: Integer -> [Integer]
primeFactorsRepeat 1 = []
primeFactorsRepeat x = firstFacotr : primeFactorsRepeat (x `div` firstFacotr)
  where
    firstFacotr = head (filter divisble primes)
    divisble    = (== 0) . (x `mod`)

splitWithOne :: (a -> Bool) -> [a] -> [[a]]
splitWithOne f xs = recurse xs []
  where
    recurse []     ps             = [ps, []]
    recurse (x:xs) ps | f x       = recurse xs (x:ps)
                      | otherwise = [ps, x:xs]

splitSame :: Eq t => [t] -> [[t]]
splitSame []     = []
splitSame (x:xs) = [same] <> splitSame diff
  where (same:diff:_) = splitWithOne (== x) (x:xs)


numOfFactors :: Num a => Integer -> Int
numOfFactors = product . ((+ 1) . length <$>) . splitSame . primeFactorsRepeat


main :: IO ()
main = print . head . dropWhile ((< 500) . numOfFactors) $ triNums
