{-# LANGUAGE FlexibleContexts #-}

import qualified Data.Vector as V

import Control.Monad.Memo    as MT


-- does not save the path in order to test memoization
longestPath' :: (Ord v, Num v) => Int -> Int -> V.Vector (V.Vector v) -> v
longestPath' x y vec = recurse x y
  where recurse x y
          | y == 0      = loc + loc
          | x == 0      = loc + up
          | x == rowLen = loc + upleft
          | otherwise  = max (loc + up) (loc + upleft)
          where
            loc    = vec V.! y V.! x
            rowLen = length (vec V.! y) - 1
            up     = recurse x       (y - 1) 
            upleft = recurse (x - 1) (y - 1)

-- does not save the path in order to test memoization
-- this actually memoizaes
longestPathMem' :: Int -> Int -> V.Vector (V.Vector Integer) -> Integer
longestPathMem' x y vec = startEvalMemo $ recurse x y
  where
    recurse :: MonadMemo (Int, Int) Integer m => Int -> Int -> m Integer
    recurse x y
      | y == 0      = return $ loc + loc
      | x == 0      = (+ loc) <$> up
      | x == rowLen = (+ loc) <$> upleft
      | otherwise  = do
          upp <- up
          upl <- upleft
          return $ max (loc + upp) (loc + upl)
      where
        loc    = vec V.! y V.! x
        rowLen = length (vec V.! y) - 1
        up     = for2 memo recurse x       (y - 1)
        upleft = for2 memo recurse (x - 1) (y - 1)
