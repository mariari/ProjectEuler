

answer :: [Integer]
answer = floor <$> head [[x,y,z] | x <- [1..1000],
                               y <- [1..(x - 1)],
                               let z = sqrt (x^2 + y^2),
                                   x + y + z == 1000]


-- from somewhere... it cheats
triplets :: Integral t => t -> [[t]]
triplets l = [[a,b,c] | m <- [2..limit],
                        n <- [1,3..(m-1)], -- has to be odd
                        let a = m^2 - n^2,
                        let b = 2*m*n,
                        let c = m^2 + n^2,
                        a+b+c==l]
    where limit = floor . sqrt . fromIntegral $ l

problem_9 = product . head . triplets $ 1000


main :: IO ()
main =  print $ product answer
