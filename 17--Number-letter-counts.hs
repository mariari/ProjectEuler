import qualified Data.Map.Strict as Map
import Data.Maybe (catMaybes)
import Data.Monoid ((<>))

ones :: Map.Map Integer String
ones = Map.fromList [(1, "one"),   (2,"two"),     (3, "three")
                    ,(4, "four"),  (5, "five"),   (6, "six")
                    ,(7, "seven"), (8, "eight"),  (9, "nine")
                    ,(0, "")]

onesDigits :: Integer -> Maybe String
onesDigits x = Map.lookup x ones

tens :: Map.Map Integer String
tens = Map.fromList [(1, "ten"),   (2, "twen"), (3, "thir")
                    ,(4, "for"),   (5, "fif"),  (6, "six")
                    ,(7, "seven"), (8, "eigh"), (9, "nine")]

tensLookup :: Integer -> Maybe String
tensLookup x = Map.lookup x tens

tensDigits :: Integer -> Maybe String
tensDigits 10 = Just "ten"
tensDigits 11 = Just "eleven"
tensDigits 12 = Just "twelve"
tensDigits 14 = Just "fourteen"
tensDigits x
  | x < 10    = onesDigits x
  | x < 20    = (<> "teen") <$> tensLookup (x `mod` 10) -- grab ones to see what we have to append teen to
  | x < 100   = do ten <- tensLookup (x `div` 10)
                   one <- onesDigits (x `mod` 10)
                   return $ ten <> "ty-" <> one
  | otherwise = Nothing

-- can abstract out hundredsDigits logic and thousandDigits logic into a common form
hundredsDigits :: Integer -> Maybe String
hundredsDigits x | x < 100 = tensDigits x
                 | x > 999 = Nothing

hundredsDigits x = do
  hund  <- onesDigits hunds
  teens <- tensDigits lower
  return $ hund <> " hundred" <> divisble <> teens
  where (hunds,lower) = x `divMod` 100
        divisble | lower == 0 = ""        -- if it's just 100 or 200 we don't need the and part
                 | otherwise = " and "

thousandsDigits :: Integer -> Maybe String
thousandsDigits x | x < 1000 = hundredsDigits x
                  | x > 9999 = Nothing

thousandsDigits x = do
  thousands <- hundredsDigits thous
  hundreds  <- hundredsDigits lower
  return $ thousands <> " thousand " <> hundreds
  where (thous,lower) = x `divMod` 1000

toDigits :: Integer -> Maybe String
toDigits = thousandsDigits

sumDigits :: Integer -> Int
sumDigits x = sum . fmap length . (filter (\x -> x >= 'a' && x <= 'z') <$>) . catMaybes . fmap toDigits $ [1..x]

main :: IO ()
main = print $ sumDigits 1000
