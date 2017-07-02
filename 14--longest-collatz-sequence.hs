{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}

import Data.Ord
import Data.List
import Control.Monad.Memo


--data Col a = Col { unCol :: a }


data Col where
  Col :: Num a => a -> Col


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


collatzMem :: MonadMemo Integer Collatz f => Integer -> f Collatz
collatzMem 1 = return $ Collatz 1 [1] 1
collatzMem x = Collatz x . ((x :) . lis) <*> ((1 +) . len) <$> coll
    where coll
            | even x    = memo collatzMem (x `div` 2)
            | otherwise = memo collatzMem (3 * x + 1)


collatz' :: Integer -> Collatz
collatz' = startEvalMemo . collatzMem


-- Now this function is properly memoized

-- uses 3 GB with collatz and computers in 7 seconds
-- with the -prof -fprof-auto -rtsopts flags, it only takes up 140MB...
-- WHY!??!?

-- stack ghc -- -prof -fprof-auto -rtsopts -O2 14--longest-collatz-sequence.hs -o collatz 
-- ./collatz +RTS -hc -p
-- hp2ps collatz.hp
-- evince collatz.ps
-- takes longer than a minute with memoCollatz


maxCollatz :: Integer -> Collatz
maxCollatz x = maximumBy (comparing len) . startEvalMemo . traverse collatzMem $ [1,3..x]


main :: IO ()
main = print $ maxCollatz 999999
