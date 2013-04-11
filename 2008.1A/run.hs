import Data.List (sort)

type Vector = [Integer]
type Case = (Integer, Vector, Vector)

-- Import
caseify :: Integer -> [String] -> [Case]
caseify n [] = []
caseify n (_:v1:v2:rest) = (n, vectorize v1, vectorize v2):(caseify (n + 1) rest)

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
-- dataImport = lines . tail . caseify
dataImport s = caseify 1 $ tail $ lines s

-- Go
main = do
  inFile <- getArgs
  putStrLn $ show inFile
  input <- readFile inFile
--  putStrLn $ show $ dataImport $ take 100 input
  putStrLn $ unlines $ map prettyCase $ dataImport input
