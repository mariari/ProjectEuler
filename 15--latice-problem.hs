
choose :: Integral t => t -> t -> t
choose _ 0 = 1
choose 0 _ = 0
choose n k = choose (n-1) (k-1) * n `div` k 

main :: IO ()
main = print $ 40 `choose` 20
