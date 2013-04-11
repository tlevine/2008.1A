import Data.List (sort)

type Vector = [Integer]
type Case = (Vector, Vector)

-- Import
caseify :: [String] -> [Case]
caseify [] = []
caseify (n:v1:v2:rest) = (vectorize v1, vectorize v2):(caseify rest)

vectorize :: String -> Vector
vectorize v = map (\w -> (read w :: Integer)) $ words v

-- Scalar product
minimumScalarProduct :: (Vector, Vector) -> Integer
minimumScalarProduct (v1, v2) = sum $ (zipWith (*)) (sort v1) (reverse $ sort v2)

-- Go
main = do
  input <- readFile "A-small-practice.in"
  -- putStrLn $ show $ caseify $ tail $ lines input
  -- putStrLn $ show $ minimumScalarProduct caseify $ tail $ take (9 + 1) $ lines input
  putStrLn input
