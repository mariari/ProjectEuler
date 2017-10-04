import System.IO as I
import Data.List.Split
import Data.Char
import Data.List

file :: IO String
file =  readFile "p022_names.txt"

sortWords :: String -> [String]
sortWords = sort . splitOn "," . filter (/= '"')

valWord :: String -> Int
valWord = sum . fmap (subtract 64 . ord)


main :: IO ()
main = file >>= print . sum . zipWith (*) [1..] . (valWord <$>) . sortWords
