import Data.List (sort)

type Vector = [Integer]
type Case = (Integer, Vector, Vector)

-- Import
caseify :: [String] -> [Case]
caseify [] = []
caseify (n:v1:v2:rest) = ((read n :: Integer), vectorize v1, vectorize v2):(caseify rest)

vectorize :: String -> Vector
vectorize v = map (\w -> (read w :: Integer)) $ words v

-- Scalar product
minimumScalarProduct :: Vector -> Vector -> Integer
minimumScalarProduct v1 v2 = sum $ (zipWith (*)) (sort v1) (reverse $ sort v2)

-- Print
prettyCase :: Case -> String
prettyCase (n,v1,v2) = "Case #" ++ (show n) ++ ": " ++ (show msp)
  where
    msp = minimumScalarProduct v1 v2

dataImport :: String -> [Case]
dataImport s = caseify $ lines $ tail s

-- Go
main = do
  input <- readFile "A-small-practice.in"
  putStrLn $ unlines $ map prettyCase $ dataImport input
