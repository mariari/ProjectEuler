import Data.Function.Memoize as M
import Data.MemoTrie         as MT
import qualified Data.Vector as V

vec = V.fromList <$> V.fromList [  [08, 02, 22, 97, 38, 15, 00, 40, 00, 75, 04, 05, 07, 78, 52, 12, 50, 77, 91, 08]
                                ,  [49, 49, 99, 40, 17, 81, 18, 57, 60, 87, 17, 40, 98, 43, 69, 48, 04, 56, 62, 00]
                                ,  [81, 49, 31, 73, 55, 79, 14, 29, 93, 71, 40, 67, 53, 88, 30, 03, 49, 13, 36, 65]
                                ,  [52, 70, 95, 23, 04, 60, 11, 42, 69, 24, 68, 56, 01, 32, 56, 71, 37, 02, 36, 91]
                                ,  [22, 31, 16, 71, 51, 67, 63, 89, 41, 92, 36, 54, 22, 40, 40, 28, 66, 33, 13, 80]
                                ,  [24, 47, 32, 60, 99, 03, 45, 02, 44, 75, 33, 53, 78, 36, 84, 20, 35, 17, 12, 50]
                                ,  [32, 98, 81, 28, 64, 23, 67, 10, 26, 38, 40, 67, 59, 54, 70, 66, 18, 38, 64, 70]
                                ,  [67, 26, 20, 68, 02, 62, 12, 20, 95, 63, 94, 39, 63, 08, 40, 91, 66, 49, 94, 21]
                                ,  [24, 55, 58, 05, 66, 73, 99, 26, 97, 17, 78, 78, 96, 83, 14, 88, 34, 89, 63, 72]
                                ,  [21, 36, 23, 09, 75, 00, 76, 44, 20, 45, 35, 14, 00, 61, 33, 97, 34, 31, 33, 95]
                                ,  [78, 17, 53, 28, 22, 75, 31, 67, 15, 94, 03, 80, 04, 62, 16, 14, 09, 53, 56, 92]
                                ,  [16, 39, 05, 42, 96, 35, 31, 47, 55, 58, 88, 24, 00, 17, 54, 24, 36, 29, 85, 57]
                                ,  [86, 56, 00, 48, 35, 71, 89, 07, 05, 44, 44, 37, 44, 60, 21, 58, 51, 54, 17, 58]
                                ,  [19, 80, 81, 68, 05, 94, 47, 69, 28, 73, 92, 13, 86, 52, 17, 77, 04, 89, 55, 40]
                                ,  [04, 52, 08, 83, 97, 35, 99, 16, 07, 97, 57, 32, 16, 26, 26, 79, 33, 27, 98, 66]
                                ,  [88, 36, 68, 87, 57, 62, 20, 72, 03, 46, 33, 67, 46, 55, 12, 32, 63, 93, 53, 69]
                                ,  [04, 42, 16, 73, 38, 25, 39, 11, 24, 94, 72, 18, 08, 46, 29, 32, 40, 62, 76, 36]
                                ,  [20, 69, 36, 41, 72, 30, 23, 88, 34, 62, 99, 69, 82, 67, 59, 85, 74, 04, 36, 16]
                                ,  [20, 73, 35, 29, 78, 31, 90, 01, 74, 31, 49, 71, 48, 86, 81, 16, 23, 57, 05, 54]
                                ,  [01, 70, 54, 71, 83, 51, 54, 69, 16, 92, 33, 48, 61, 43, 52, 01, 89, 19, 67, 48]]

len = V.length vec
-- use maximum maximum [(34,[1,2,3]), (32,[3,2,2])] -> (34,[1,2,3])

-- calling memoize2 on recFormulaMem slows it down!
recFormula :: (Num a, Eq a) => Int -> Int -> a -> (Integer, [Integer])
recFormula i j left = memo2 recFormulaMem i j left  (1, [])
  where 
    recFormulaMem _    _    0 tuple                  = tuple
    recFormulaMem (-1) _    p tuple                  = tuple
    recFormulaMem _    (-1) p tuple                  = tuple
    recFormulaMem i    j    p (acc, xs) | j >= len    = (acc, xs)
                                        | otherwise  = maximum [move (i - 1) (j - 1) 
                                                               ,move (i - 1)    j
                                                               ,move    i    (j - 1)
                                                               ,move (i - 1) (j + 1)]
      where val = vec V.! i V.! j
            new = (acc * val, val : xs)
            move i j = recFormulaMem i j (p - 1) new





greatestMult :: (Num a, Eq a) => a -> (Integer, [Integer])
greatestMult left = maximum [recFormula i j left | i <- [0..lenI], j <- [0..lenJ]]
  where lenI = V.length vec - 1
        lenJ = V.length (vec V.! 0) - 1 


main :: IO ()
main = print . fst . greatestMult $ 4



recFormulaMem f _    _    0 tuple     = tuple
recFormulaMem f (-1) (-1) p tuple     = tuple
recFormulaMem f (-1) _    p tuple     = tuple
recFormulaMem f _    (-1) p tuple     = tuple
recFormulaMem f i    j    p (acc, xs) = maximum [f (i - 1) (j - 1) (p - 1) new
                                                ,f (i - 1)    j    (p - 1) new
                                                ,f    i    (j - 1) (p - 1) new]
  where val = vec V.! i V.! j
        new = (acc * val, val : xs)


formMemo = MT.memoFix (memo2 . recFormulaMem)


-- maximum (formMemo' 10) -> 15.94 seconds...
formMemo' left = (\x -> (\y -> recFormula x y left) <$> [0..lenI]) =<< [0..lenJ]
  where lenI = V.length vec - 1
        lenJ = V.length (vec V.! 0) - 1  

greatestMultMemo' = maximum . formMemo'

greatestMultMemo left = maximum [formMemo i j left (1, []) | i <- [0..lenI], j <- [0..lenJ]]
  where lenI = V.length vec - 1
        lenJ = V.length (vec V.! 0) - 1 
