import qualified Data.List.Split as Split
import qualified Data.Char       as Char
import qualified Data.Set        as Set

type Point = (Int, Int, Int)
to3tup [a, b, c] = (a, b, c)
parsePoint s = to3tup $ (read :: String -> Int) <$> Split.splitWhen (not . Char.isNumber) s
parsePoints = fmap parsePoint

neighbors points (x, y, z) =
  let np = [(x + 1, y, z), (x - 1, y, z), (x, y + 1, z), (x, y - 1, z), (x, y, z - 1), (x, y, z + 1)] in
  foldl (\acc b -> if b then acc + 1 else acc) 0 $ fmap (`Set.member` points) np

surface points i p = i + 6 - neighbors points p

part1 pointList = do
  let pointSet = Set.fromList pointList
  foldl (surface pointSet) 0 pointSet

main = do
  pointsE1 <- parsePoints . lines <$> readFile "input_e1.txt"
  print $ part1 pointsE1
  points <- parsePoints . lines <$> readFile "input.txt"
  print $ part1 points

