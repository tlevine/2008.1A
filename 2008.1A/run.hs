v1 :: [Integer]
v1 = [1..3]

v2 :: [Integer]
v2 = [0..2]


scalarProduct :: [Integer] -> [Integer] -> [Integer]
scalarProduct = zipWith (*)


main = do
  putStrLn $ show $ scalarProduct v1 v2
