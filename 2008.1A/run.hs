import Data.List (sort)

type Vector = [Integer]
type Case = (Integer, Vector, Vector)

-- Import
dataImport :: String -> [Case]
dataImport = ((caseify 1) . tail) . lines

caseify :: Integer -> [String] -> [Case]
caseify n [] = []
caseify n (_:v1:v2:rest) = (n, vectorize v1, vectorize v2):(caseify (n + 1) rest)

vectorize :: String -> Vector
vectorize = (map (\w -> (read w :: Integer))) . words

-- Scalar product
minimumScalarProduct :: Vector -> Vector -> Integer
minimumScalarProduct v1 v2 = sum $ (zipWith (*)) (sort v1) (reverse $ sort v2)

-- Output
prettyCase :: Case -> String
prettyCase (n,v1,v2) = "Case #" ++ (show n) ++ ": " ++ (show msp)
  where
    msp = minimumScalarProduct v1 v2

-- Go
go = unlines . (map prettyCase) . dataImport
main = do
  small <- readFile "A-small-practice.in"
  writeFile "A-small-practice.out" (go small)

  large <- readFile "A-large-practice.in"
  writeFile "A-large-practice.out" (go large)
