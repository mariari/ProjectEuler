import System.IO as I
import Data.List.Split
import Data.Char
import Data.List

file :: IO String
file =  readFile "p022_names.txt"

wsortedWords :: IO [String]
sortedWords = (sort . (filter (/= '"') <$>) . splitOn ",") <$> file

wordVal :: IO [Int]
wordVal = (fmap . fmap) (sum . ((\x -> x - 64) . ord <$>)) sortedWords


main = wordVal >>= print . sum . zipWith (*) [1..] 
