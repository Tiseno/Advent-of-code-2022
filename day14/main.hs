import Data.Matrix

data CaveCell = Rock | Sand | Empty
instance Show CaveCell where
  show Rock = "#"
  show Sand = "O"
  show Empty = " "

parsePath = id
parsePaths = fmap parsePath

type Cave = Matrix CaveCell
createCaveSystem :: [a] -> Cave
createCaveSystem paths = Data.Matrix.matrix (length paths) (length paths) (const Rock)

part1 input = do
  let paths = parsePaths input
  let cave = createCaveSystem paths
  cave

main = do
  inputE1 <- lines <$> readFile "input_e1.txt"
  input <- lines <$> readFile "input.txt"
  print $ part1 inputE1
