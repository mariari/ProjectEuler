
choose :: Integral t => t -> t -> t
choose n 0 = 1
choose 0 k = 0
choose n k = choose (n-1) (k-1) * n `div` k 

main :: IO ()
main = print $ 40 `choose` 20
