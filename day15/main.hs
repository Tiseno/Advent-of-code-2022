import qualified Debug.Trace as Trace
import qualified Data.Char as Char
import qualified Data.List.Split as Split
import qualified Data.List as List
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set
import GHC.Float (int2Float, float2Int)

debug n = Trace.traceShow n n

type Point = (Int, Int)
type SensorData = (Point, Point)

parseSensorData :: String -> SensorData
parseSensorData s = ((sx, sy), (bx, by))
  where
    [sx, sy, bx, by] = fmap (read :: String -> Int) $ filter (/="") $ Split.splitWhen (not . isNumberOrDash) s
    isNumberOrDash c = Char.isNumber c || c == '-'

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

sortByX :: [(Int, Int)] -> [(Int, Int)]
sortByX = List.sortBy (\(l1,_) (l2, _) -> compare l1 l2)

part1 y sensorData = do
  let ranges = Maybe.mapMaybe (rangeAtY y) sensorData
  let sortedRanges = sortByX ranges
  let foldedRanges = foldlRanges [] sortedRanges
  sum $ fmap rangeLength foldedRanges

type Line = (Point, Point)

manhattan :: Point -> Point -> Int
manhattan (xs, ys) (xb, yb) = abs (xs - xb) + abs (ys - yb)

surroundingLines :: SensorData -> [Line]
surroundingLines sd@(sensor@(sx, sy), beacon) =
  [ ((sx, sy + d), (sx + d, sy))
  , ((sx + d, sy), (sx, sy - d))
  , ((sx, sy - d), (sx - d, sy))
  , ((sx - d, sy), (sx, sy + d))
  ] where d = uncurry manhattan sd + 1

surroundingLinesExclusive :: SensorData -> [Line]
surroundingLinesExclusive sd@(sensor@(sx, sy), beacon) =
  [ ((sx + 1, sy + d - 1), (sx + d - 1, sy + 1))
  , ((sx + d - 1, sy + 1), (sx + 1, sy - d - 1))
  , ((sx + 1, sy - d - 1), (sx - d - 1, sy + 1))
  , ((sx - d - 1, sy + 1), (sx + 1, sy + d - 1))
  ] where d = uncurry manhattan sd + 1

type Box = (Point, Point)

pointInside :: (Point, Point) -> Point -> Bool
pointInside ((x1, y1), (x2, y2)) (x, y) = min x1 x2 <= x && x <= max x1 x2 && min y1 y2 <= y && y <= max y1 y2

xIntersectionPoint :: Int -> Line -> Point
xIntersectionPoint x (p1, p2) =
  let ((x1, y1), (x2, y2)) = if fst p1 < fst p2 then (p1, p2) else (p2, p1) in
  let dy = (y2 - y1) `div` (x2 - x1) in
  (x, y1 + dy*(x - x1))

yIntersectionPoint :: Int -> Line -> Point
yIntersectionPoint y (p1, p2) =
  let ((x1, y1), (x2, y2)) = if snd p1 < snd p2 then (p1, p2) else (p2, p1) in
  let dx = (x2 - x1) `div` (y2 - y1) in
  (x1 + dx*(y - y1), y)

pointInsideBoxAndLine :: Box -> Line -> Point -> Bool
pointInsideBoxAndLine box line point = pointInside box point && pointInside line point

boxIntersectionPoints :: Box -> Line -> [Point]
boxIntersectionPoints box@((left, top), (right, bot)) line =
  Set.elems $ Set.fromList $ filter (pointInsideBoxAndLine box line) [xIntersectionPoint left line, xIntersectionPoint right line, yIntersectionPoint top line, yIntersectionPoint bot line]

fitLineToBox :: Box -> Line -> Maybe Line
fitLineToBox box line@(p1, p2) =
  let insidePoints = filter (pointInside box) [p1, p2] in
  if length insidePoints == 2 then Just line else
  let intersections = boxIntersectionPoints box line in
    if length intersections == 2 then let [i1, i2] = intersections in Just (i1, i2) else
    if length intersections == 1 then
      if length insidePoints == 1 then Just (head insidePoints, head intersections)
      else Just (head intersections, head intersections)
    else Nothing

pointInsideSensorRange :: SensorData -> Point -> Bool
pointInsideSensorRange (sensor, beacon) point = manhattan sensor beacon >= manhattan sensor point

lineLineIntersection l1 l2 = Nothing

sensorDataIntersectionPointsOld :: SensorData -> Line -> [Point]
sensorDataIntersectionPointsOld sd line =
  Set.elems $ Set.fromList $ Maybe.mapMaybe (lineLineIntersection line) (surroundingLinesExclusive sd)

fitLineToSensorDataOld :: SensorData -> Line -> [Line]
fitLineToSensorDataOld sd line@(p1, p2) =
  let (insidePoints, outsidePoints) = List.partition (pointInsideSensorRange sd) [p1, p2] in
  if length insidePoints == 2 then [] else
  let intersections = sensorDataIntersectionPointsOld sd line in
  if length intersections == 1 then [(head outsidePoints, head intersections)] else
  if null intersections then [line] else -- there must be two intersections and two outside points
    let [a, b, c, d] = sortByX $ intersections ++ outsidePoints in [(a, b), (c, d)]

pointOnLine :: Line -> Point -> Bool
pointOnLine ((sx, sy), (ex, ey)) (x, y) = min sx ex <= x && x <= max sx ex && min sy ey <= y && y <= max sy ey

sensorDataIntersectionPointsPositive :: SensorData -> Line -> [Point]
sensorDataIntersectionPointsPositive sd@((sx, sy), _) line@(lineStart, _) =
  let d = uncurry manhattan sd + 1 in
  let slope1 = ((sx - d + 1, sy - 1), (sx - 1, sy - d + 1)) in
  let slope2 = ((sx + 1, sy + d - 1), (sx + d - 1, sy + 1)) in
  let intersection1 = inter False (fst slope1) lineStart in
  let intersection2 = inter True (fst slope2) lineStart in
  filter (pointOnLine line) [intersection1, intersection2]
    where
      inter :: Bool -> Point -> Point -> Point
      inter r (slopeStartx, slopeStarty) (lineStartx, lineStarty) =
        let dx2 = (int2Float(slopeStarty - lineStarty - slopeStartx + lineStartx) / 2) in
        ((if r then ceiling else floor) $ int2Float slopeStartx + dx2, (if r then ceiling else floor) $ int2Float slopeStarty - dx2)

sensorDataIntersectionPointsNegative sd@((sx, sy), _) line@(lineStart, _) =
  let d = uncurry manhattan sd in
  let slope1 = ((sx - d + 1, sy + 1), (sx - 1, sy + d - 1)) in
  let slope2 = ((sx + 1, sy - d + 1), (sx + d - 1, sy - 1)) in
  let intersection1 = inter False (fst slope1) lineStart in
  let intersection2 = inter True (fst slope2) lineStart in
  filter (pointOnLine line) [intersection1, intersection2]
    where
      inter :: Bool -> Point -> Point -> Point
      inter r (slopeStartx, slopeStarty) (lineStartx, lineStarty) =
        let dx2 = (int2Float(-slopeStarty + lineStarty - slopeStartx + lineStartx) / 2) in
        ((if r then ceiling else floor) $ int2Float slopeStartx + dx2, (if r then floor else ceiling) $ int2Float slopeStarty + dx2)

sensorDataIntersectionPoints :: SensorData -> Line -> [Point]
sensorDataIntersectionPoints sd line@(p1, p2) = (if snd p1 < snd p2 then sensorDataIntersectionPointsPositive else sensorDataIntersectionPointsNegative) sd line

fitLineToSensorData :: SensorData -> Line -> [Line]
fitLineToSensorData sd (l1, l2) =
  let line@(p1, p2) = if fst l1 < fst l2 then (l1, l2) else (l2, l1) in
  let (insidePoints, outsidePoints) = List.partition (pointInsideSensorRange sd) [p1, p2] in
  if null outsidePoints then [] else
  let intersections = sensorDataIntersectionPoints sd line in
  if length intersections == 2 then let [i1, i2] = intersections in [(p1, i1), (i2, p2)] else
  if length intersections == 1 then let [i] = intersections in [(head outsidePoints, i)] else [line]

fitLinesToSensorData :: [Line] -> SensorData -> [Line]
fitLinesToSensorData lines sd = concatMap (fitLineToSensorData sd) lines

part2 bound sensorData = do
  let lines = concatMap surroundingLines sensorData
  let linesInsideBox = fmap (fitLineToBox ((0, 0), (bound, bound))) lines
  let fitLines = foldl fitLinesToSensorData lines sensorData
  let insideBox = filter (\l -> pointInside ((0,0), (bound, bound)) (fst l) && pointInside ((0,0), (bound, bound)) (snd l)) fitLines
  let ((x, y), _) = last insideBox
  4000000 * x + y

main = do
  sensorDataE1 <- parseSensorDatas . lines <$> readFile "input_e1.txt"
  sensorData <- parseSensorDatas . lines <$> readFile "input.txt"
  print $ part1 10 sensorDataE1
  print $ part1 2000000 sensorData
  print $ part2 20 sensorDataE1
  print $ part2 4000000 sensorData

