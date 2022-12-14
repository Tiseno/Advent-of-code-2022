import Data.Char
import Data.List
import Data.Matrix
import Data.Set as Set

type Point = (Int, Int)

elemPoint :: Eq a => a -> [[a]] -> Point
elemPoint a m =
  let (Just x) = Data.List.findIndex (elem a) m in
  let (Just y) = elemIndex a (m !! x) in
  (x + 1, y + 1)

charToHeight :: Char -> Int
charToHeight c = let c2 | c == 'S' = 'a' | c == 'E' = 'z' | otherwise = c in ord c2 - 97

initialDistance :: Char -> Char -> Int
initialDistance start c | c == start = 0 | otherwise = 100000000000000

type DistFunc = Point -> Point -> Int

cliffDistance :: Matrix Int -> DistFunc
cliffDistance heightMap from to =
  let fromCliff = uncurry getElem from heightMap in
  let toCliff = uncurry getElem to heightMap in
  if toCliff > fromCliff + 1 then 100000000000000 else 1

type PrioPoint = (Int, Point)

prioInsert :: PrioPoint -> [PrioPoint] -> [PrioPoint]
prioInsert p [] = [p]
prioInsert p (x:xs) = if fst p < fst x then p:x:xs else x : prioInsert p xs

type DState = (Matrix Int, Matrix Point, [PrioPoint], Set.Set Point)

unvisitedNeighborsOf :: Set.Set Point -> Int -> Int -> Point -> [Point]
unvisitedNeighborsOf visited xmax ymax (x, y) =
  let n = [ (x+1, y), (x, y+1), (x, y-1), (x-1, y) ] :: [Point] in
  let inBounds = Data.List.filter (\(x, y) -> x >= 1 && y >= 1 && x <= xmax && y <= ymax) n in
  Data.List.filter (`notElem` visited) inBounds

visitNeighbor :: DistFunc -> Point -> DState -> Point -> DState
visitNeighbor distFunc u dstate@(dist, path, queue, visited) n =
  let newDistance = uncurry getElem u dist + distFunc u n in
  if newDistance >= uncurry getElem n dist then dstate else
  (setElem newDistance n dist, setElem u n path, prioInsert (newDistance, n) queue, visited)

shortestPathsR :: DistFunc -> DState -> DState
shortestPathsR distFunc dstate@(_, _, [], _) = dstate
shortestPathsR distFunc dstate@(dist, path, queue, visited) =
  let (_, u):queue2 = queue in
  if u `elem` visited then shortestPathsR distFunc (dist, path, queue2, visited) else
  let visited2 = Set.insert u visited in
  let ns = unvisitedNeighborsOf visited (nrows dist) (ncols dist) u in
  shortestPathsR distFunc $ Data.List.foldl (visitNeighbor distFunc u) (dist, path, queue2, visited2) ns

part1 chars =
  let charMatrix = fromLists chars in
  let heightMap = fmap charToHeight charMatrix in
  let distances = fmap (initialDistance 'S') charMatrix in
  let paths = mapPos const distances in
  let start = elemPoint 'S' chars in
  let end = elemPoint 'E' chars in
  let (d, _, _, _) = shortestPathsR (cliffDistance heightMap) (distances, paths, [(0, start)], Set.empty) in
  uncurry getElem end d

fmapi :: (Int -> a -> b) -> [a] -> [b]
fmapi f list = snd $ Data.List.foldl (\(i, b) a -> (i+1, b ++ [f i a])) (0, []) list

elemPoints :: Eq a => a -> [[a]] -> [Point]
elemPoints a m = fmap fst $ Data.List.filter (\(p, c) -> c == a) $ concat $ fmapi (\i row -> fmapi (\j e -> ((i + 1, j + 1), e)) row) m

cliffDistanceReverse :: Matrix Int -> DistFunc
cliffDistanceReverse heightMap from to =
  let fromCliff = uncurry getElem from heightMap in
  let toCliff = uncurry getElem to heightMap in
  if toCliff + 1 < fromCliff then 100000000000000 else 1

part2 chars =
  let charMatrix = fromLists chars in
  let heightMap = fmap charToHeight charMatrix in
  let distances = fmap (initialDistance 'E') charMatrix in
  let paths = mapPos const distances in
  let start = elemPoint 'S' chars in
  let end = elemPoint 'E' chars in
  let aPoints = elemPoints 'a' chars in
  let (d, _, _, _) = shortestPathsR (cliffDistanceReverse heightMap) (distances, paths, [(0, end)], Set.empty) in
  let aDistances = sort $ fmap (flip (uncurry getElem) d) aPoints in
  head aDistances

main = do
  charsE1 <- lines <$> readFile "input_e1.txt"
  chars <- lines <$> readFile "input.txt"
  print $ part1 charsE1
  print $ part1 chars
  print $ part2 charsE1
  print $ part2 chars

