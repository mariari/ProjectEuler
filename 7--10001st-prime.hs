import           Data.List.Ordered

primes :: [Integer]
primes = 2 : 3 : [5,7..] `minus` unionAll [[p*p, p*p+2*p..] | p <- tail primes]

main :: IO ()
main = print $ primes !! 10000
