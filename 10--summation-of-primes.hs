import           Data.List.Ordered

primes :: [Integer]
primes = 2 : 3 : [5,7..] `minus` unionAll [[p*p, p*p+2*p..] | p <- tail primes]


-- solves the problem of the current prime + next being < 2000000
foldUnitl :: (Ord t, Num t) => t -> [t] -> t
foldUnitl cap xs = recurse cap xs 0
  where
    recurse cap (x1:x2:xs) acc
          | x1 + x2 > cap = x1
          | otherwise     = recurse cap xs (x1 + x2)

-- just the current one is below
foldUnitl' :: (Foldable t1, Ord t, Num t) => t -> t1 t -> t
foldUnitl' cap  = foldr1 add
  where add x y
          | x > cap   = 0
          | otherwise = x + y


main :: IO ()
main = print $ foldUnitl' 2000000 primes


-- ================================================== Broken Code ================================================== --
-- solves the wrong problem
foldUnitl'' :: (Foldable t1, Ord t, Num t) => t -> t1 t -> t
foldUnitl'' cap  = foldr1 add
  where add x y
          | x + y > cap = x -- the + y makes fodr1 not be able to lazily evaluate
          | otherwise   = x + y
