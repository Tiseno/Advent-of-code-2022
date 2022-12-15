import qualified Data.Char as Char
import qualified Data.List.Split as Split
import qualified Data.List as List
import qualified Data.Maybe as Maybe

type Point = (Int, Int)
type SensorData = (Point, Point)

parseSensorData :: String -> SensorData
parseSensorData s = let [sx, sy, bx, by] = fmap (read :: String -> Int) $ filter (/="") $ Split.splitWhen (not . Char.isNumber) s in
  ((sx, sy), (bx, by))

parseSensorDatas :: [String] -> [SensorData]
parseSensorDatas = fmap parseSensorData

coverAtY y ((xs, ys), (xb, yb)) =
  let distanceToB = (abs (xs - xb) + abs (ys - yb)) in
  let distanceToY = abs (ys - y) in
  if distanceToY > distanceToB then Nothing else
  let coverHalfSize =  distanceToB - distanceToY in
  Just (xs - abs coverHalfSize, xs + abs coverHalfSize)

combineCovers c1@(l1, r1) c2@(l2, r2) = if r1 < l2 || r2 < l1 then [c1, c2] else [(min l1 l2, max r1 r2)]

foldlCovers covers [] = covers
foldlCovers [] (x:xs) = foldlCovers [x] xs
foldlCovers (prev:covers) (x:xs) = foldlCovers (combineCovers x prev ++ covers) xs

coverLength (l, r) = abs (l - r)

part1 y sensorData = do
  let covers = Maybe.mapMaybe (coverAtY y) sensorData
  let sortedCovers = List.sortBy (\(l1,_) (l2, _) -> compare l1 l2) covers
  let foldedCovers = foldlCovers [] sortedCovers
  sum $ fmap coverLength foldedCovers

main = do
  sensorDataE1 <- lines <$> readFile "input_e1.txt"
  print $ part1 10 $ parseSensorDatas sensorDataE1
  sensorData <- lines <$> readFile "input.txt"
  print $ part1 2000000 $ parseSensorDatas sensorData

