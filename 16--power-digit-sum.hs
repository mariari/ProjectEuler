import Data.Digits


main :: IO ()
main = print . sum . digits 10 $ 2^1000
