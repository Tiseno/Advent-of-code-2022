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

initialDistance :: Char -> Int
initialDistance c | c == 'S' = 0 | otherwise =  100000000000000

cliffDistance :: Matrix Int -> Point -> Point -> Int
cliffDistance heightMap from to =
  let fromCliff = uncurry getElem from heightMap in
  let toCliff = uncurry getElem to heightMap in
  if toCliff > fromCliff + 1 then 100000000000000 else 1

type PrioPoint = (Int, Point)

prioInsert :: PrioPoint -> [PrioPoint] -> [PrioPoint]
prioInsert p [] = [p]
prioInsert p (x:xs) = if fst p < fst x then p:x:xs else x : prioInsert p xs

type DState = (Matrix Int, Matrix Point, [PrioPoint], Set.Set Point, Matrix Int)

unvisitedNeighborsOf :: Set.Set Point -> Int -> Int -> Point -> [Point]
unvisitedNeighborsOf visited xmax ymax (x, y) =
  let n = [ (x+1, y), (x, y+1), (x, y-1), (x-1, y) ] :: [Point] in
  let inBounds = Data.List.filter (\(x, y) -> x >= 1 && y >= 1 && x <= xmax && y <= ymax) n in
  Data.List.filter (`notElem` visited) inBounds

visitNeighbor :: Point -> DState -> Point -> DState
visitNeighbor u dstate@(dist, path, queue, visited, heightMap) n =
  let newDistance = uncurry getElem u dist + cliffDistance heightMap u n in
  if newDistance >= uncurry getElem n dist then dstate else
  (setElem newDistance n dist, setElem u n path, prioInsert (newDistance, n) queue, visited, heightMap)

shortestPathsR :: DState -> DState
shortestPathsR dstate@(_, _, [], _, _) = dstate
shortestPathsR dstate@(dist, path, queue, visited, heightMap) =
  let (_, u):queue2 = queue in
  if u `elem` visited then shortestPathsR (dist, path, queue2, visited, heightMap) else
  let visited2 = Set.insert u visited in
  let ns = unvisitedNeighborsOf visited (nrows dist) (ncols dist) u in
  shortestPathsR $ Data.List.foldl (visitNeighbor u) (dist, path, queue2, visited2, heightMap) ns

part1 chars =
  let charMatrix = fromLists chars in
  let heightMap = fmap charToHeight charMatrix in
  let distances = fmap initialDistance charMatrix in
  let paths = mapPos const distances in
  let start = elemPoint 'S' chars in
  let end = elemPoint 'E' chars in
  let (d, _, _, _, _) = shortestPathsR (distances, paths, [(0, start)], Set.empty, heightMap) in
  uncurry getElem end d

main = do
  charsE1 <- lines <$> readFile "input_e1.txt"
  print $ part1 charsE1
  chars <- lines <$> readFile "input.txt"
  print $ part1 chars

