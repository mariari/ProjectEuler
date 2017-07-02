import Data.Digits

main :: IO ()
main = print . sum . digits 10 . product $ [1..100]
