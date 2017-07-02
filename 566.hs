import           Data.Fixed
import qualified Data.Stream as S

-- flips a cake by a certain number
flipping :: (Num a, Num t, Enum a) => t -> S.Stream (a, t)
flipping x = S.zip numOfFlips cakeNums
  where numOfFlips = S.fromList [0..]
        cakeNums   = S.iterate (+ x) 0

-- flips a cake, can eat a lot of memory because haskell tries to load the entire list
--flipCake :: Foldable t => t Double -> Integer
flipCake :: Real a => [a] -> Integer
flipCake = fst . (S.!! 1) . S.filter isFlipped . flipping . sum
  where isFlipped (_,x) = mod' x 720 == 0 -- 720 because the icing has to be on the top

flipCakeInt :: (Foldable t, Integral a) => t a -> a
flipCakeInt = ((`lcm` 720) >>= div) . sum

flipCakeInt' :: (Foldable t, Integral a) => t a -> a
flipCakeInt' x = lcm flipAmount 720 `div` flipAmount
  where flipAmount = sum x
