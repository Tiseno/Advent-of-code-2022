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

rangeAtY y ((xs, ys), (xb, yb)) =
  let distanceToB = (abs (xs - xb) + abs (ys - yb)) in
  let distanceToY = abs (ys - y) in
  if distanceToY > distanceToB then Nothing else
  let rangeHalfSize =  distanceToB - distanceToY in
  Just (xs - abs rangeHalfSize, xs + abs rangeHalfSize)

combineRanges c1@(l1, r1) c2@(l2, r2) = if r1 < l2 || r2 < l1 then [c1, c2] else [(min l1 l2, max r1 r2)]

foldlRanges ranges [] = ranges
foldlRanges [] (x:xs) = foldlRanges [x] xs
foldlRanges (prev:ranges) (x:xs) = foldlRanges (combineRanges x prev ++ ranges) xs

rangeLength (l, r) = abs (l - r)

part1 y sensorData = do
  let ranges = Maybe.mapMaybe (rangeAtY y) sensorData
  let sortedRanges = List.sortBy (\(l1,_) (l2, _) -> compare l1 l2) ranges
  let foldedRanges = foldlRanges [] sortedRanges
  sum $ fmap rangeLength foldedRanges

main = do
  sensorDataE1 <- lines <$> readFile "input_e1.txt"
  print $ part1 10 $ parseSensorDatas sensorDataE1
  sensorData <- lines <$> readFile "input.txt"
  print $ part1 2000000 $ parseSensorDatas sensorData

