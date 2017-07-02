import           Data.List.Ordered
import qualified Data.Set          as S

-- failed attempt at primes 
myPrimes :: [Integer]
myPrimes = 2 : [x | x <- [3..], p <- primes, x `mod` p /= 0]

-- taken from the Haskell wiki
primes :: [Integer]
primes = 2 : 3 : [5,7..] `minus` unionAll [[p*p, p*p+2*p..] | p <- tail primes]


primeFactorsOf :: Integer -> [Integer]
primeFactorsOf = S.toList . S.fromList . recur
   where
     recur x
       | x == 1     = []
       | otherwise = firstFacotr : primeFactorsOf (x `div` firstFacotr)
       where firstFacotr = head . filter ((== 0) . (x `mod`)) $ primes

main = print . last . primeFactorsOf $ 600851475143
