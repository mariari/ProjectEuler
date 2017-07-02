
divisorList :: [Integer]
divisorList = [11..19] -- 1..10 are redundant checks and we increment by the largest value, so we can remove 20

-- we move up by 20, as 20 is the largest number and it has to be divisible by that
divisibleByList :: [Integer]
divisibleByList = filter (\x -> and ((== 0) . (x `mod`) <$> divisorList)) [20,40..]



main :: IO ()
main  = print . head $ divisibleByList
