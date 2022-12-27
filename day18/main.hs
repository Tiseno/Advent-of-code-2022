import qualified Data.List.Split as Split
import qualified Data.Char       as Char
import qualified Data.Set        as Set

type Point = (Int, Int, Int)
parsePoint s = (\[x, y, z] -> (x, y, z)) $ (read :: String -> Int) <$> Split.splitWhen (not . Char.isNumber) s
parsePoints = fmap parsePoint

neighbors points (x, y, z) =
  let np = [(x + 1, y, z), (x - 1, y, z), (x, y + 1, z), (x, y - 1, z), (x, y, z - 1), (x, y, z + 1)] in
  foldl (\acc b -> if b then acc + 1 else acc) 0 $ fmap (`Set.member` points) np

surface points i p = i + 6 - neighbors points p

part1 pointList = do
  let pointSet = Set.fromList pointList
  foldl (surface pointSet) 0 pointSet

directNeighborsPlus p@(x, y, z) = [ (x + 1, y, z), (x - 1, y, z), (x, y + 1, z), (x, y - 1, z), (x, y, z + 1), (x, y, z - 1), (x + 1, y + 1, z), (x + 1, y - 1, z), (x - 1, y + 1, z), (x - 1, y - 1, z), (x + 1, y, z + 1), (x + 1, y, z - 1), (x - 1, y, z + 1), (x - 1, y, z - 1), (x, y + 1, z + 1), (x, y + 1, z - 1), (x, y - 1, z + 1), (x, y - 1, z - 1) ]
directNeighbors p@(x, y, z) = [ (x + 1, y, z), (x - 1, y, z), (x, y + 1, z), (x, y - 1, z), (x, y, z + 1), (x, y, z - 1) ]

directNeighborsIn points p = Set.fromList $ filter (`Set.member` points) (directNeighbors p)

notBlockedByDirectNeighborDiagonal points p1 p2 = 2 /= length (directNeighborsIn points p1 `Set.intersection` directNeighborsIn points p2)

isAdjacentTo dropletPoints point = any (`Set.member` dropletPoints) (directNeighbors point)

andd :: (a -> Bool) -> (a -> Bool) -> a -> Bool
andd f g a = f a && g a

unvisitedSurfaceNeighbors :: Set.Set Point -> Set.Set Point -> [Point] -> Point -> [Point]
unvisitedSurfaceNeighbors dropletPoints visited tentative surfacePoint =
  filter (notBlockedByDirectNeighborDiagonal dropletPoints surfacePoint `andd` isAdjacentTo dropletPoints `andd` (`Set.notMember` dropletPoints) `andd` (`Set.notMember` visited) `andd` (`Set.notMember` Set.fromList tentative) `andd` (/= surfacePoint)) (directNeighborsPlus surfacePoint)

findEnclosingPoints _ enclosingPoints [] = enclosingPoints
findEnclosingPoints dropletPoints enclosingPoints tentative =
  let current = head tentative in
  if current `Set.member` enclosingPoints
  then findEnclosingPoints dropletPoints enclosingPoints (tail tentative)
  else
    let unvisitedNeighbors = unvisitedSurfaceNeighbors dropletPoints enclosingPoints tentative current in
    findEnclosingPoints dropletPoints (Set.insert current enclosingPoints) (tail tentative ++ unvisitedNeighbors)

xMostSurfacePoint points = (x - 1, y, z)
  where
    x_ (x, _, _) = x
    (x, y, z) = foldl (\p1 p2 -> if x_ p1 < x_ p2 then p1 else p2) (head points) points

part2 points =
  let enclosingPoints = findEnclosingPoints (Set.fromList points) Set.empty [xMostSurfacePoint points] in
  sum $ fmap (length . directNeighborsIn enclosingPoints) points

main = do
  pointsE1 <- parsePoints . lines <$> readFile "input_e1.txt"
  points <- parsePoints . lines <$> readFile "input.txt"
  print $ part1 pointsE1
  print $ part1 points
  print $ part2 pointsE1
  print $ part2 points

