type Vector = [Integer]
type Case = (Vector, Vector)

v1 :: Vector
v1 = [1..3]

v2 :: Vector
v2 = [0..2]


scalarProduct :: Vector -> Vector -> Vector
scalarProduct = zipWith (*)


caseify :: [String] -> [Case]
caseify (n:v1:v2:rest) = (vectorize v1, vectorize v2):(caseify rest)
caseify [] = []

vectorize :: String -> Vector
vectorize v = map (\w -> (read w :: Integer)) $ words v
-- vectorize = words . (map (\w -> (read w :: Integer)))

main = do
  input <- readFile "A-small-practice.in"
  -- putStrLn $ show $ caseify $ tail $ lines input
  putStrLn $ show $ caseify $ tail $ take (9 + 1) $ lines input
