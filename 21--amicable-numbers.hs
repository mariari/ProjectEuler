import           Data.List.Ordered
import qualified Data.Set          as S
import           Data.Monoid

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

-- new code to 21!!!!
expEntireLisByHead :: (Num a, Functor f) => f [a] -> f [a]
expEntireLisByHead = fmap (\(x:xs) -> take (length (x:xs)) ((x ^) <$> [1..]))

-- multiplies every element of 1 inner list by the other inner lists
combineListsMult :: (Num b, Eq b) => [[b]] -> [b]
combineListsMult xss = foldr1 multRest xss
  where multRest xs ys = xs <> (lisFilt >>= ((*) <$> ys <*>))
          where lisFilt = filter (/= ys) xss

getFactorList :: Integer -> [Integer]
getFactorList x = S.toList . S.fromList . filter (\y -> (y < x) && 0 == x `rem` y)
                . combineListsMult . expEntireLisByHead . ([1] :) . splitSame . primeFactorsRepeat $ x

isAmicable :: Integer -> Bool
isAmicable x = x == ans2 && x /= ans1
  where ans1 = sum $ getFactorList x
        ans2 = sum $ getFactorList ans1

main :: IO ()
main = print . sum $ filter isAmicable [2..10000]
