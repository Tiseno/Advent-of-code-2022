import qualified Debug.Trace as Trace
import qualified Data.Maybe as Maybe
import GHC.Float (int2Float)
import GHC.Float.RealFracMethods (float2Int)
import Data.List (partition)

debu n = Trace.trace n n
debug n = Trace.traceShow n n

type Point = (Int, Int)
type Line = (Point, Point)
type Box = (Point, Point)

--https://en.wikipedia.org/wiki/Line%E2%80%93line_intersection#Given_two_points_on_each_line_segment
-- intersection :: Line -> Line -> Maybe Point
intersection ((x1i, y1i), (x2i, y2i)) ((x3i, y3i), (x4i, y4i)) = do
  let (x1, y1, x2, y2, x3, y3, x4, y4) = (int2Float x1i, int2Float y1i, int2Float x2i, int2Float y2i, int2Float x3i, int2Float y3i, int2Float x4i, int2Float y4i)
  let t = ((x1 - x3)*(y3 - y4) - (y1 - y3)*(x3 - x4)) / ((x1 - x2)*(y3 - y4) - (y1 - y2)*(x3 - x4))
  let u = ((x1 - x3)*(y1 - y2) - (y1 - y3)*(x1 - x2)) / ((x1 - x2)*(y3 - y4) - (y1 - y2)*(x3 - x4))
  let uBased = ((x3 + (u*x4 - x3)), (y3 + u*(y4 - y3)))
  let tBased = ((x1 + (t*x2 - x1)), (y1 + t*(y2 - y1)))
  if 0 <= t && t <= 1 && 0 <= u && u <= 1 then Just (uBased, tBased) else Nothing

pointInsideBox box@((x1, y1), (x2, y2)) point@(px, py) =
  let (left, top, right, bot) = (min x1 x2, min y1 y2, max x1 x2, max y1 y2) in
  left <= px && px <= right && top <= py && py <= bot

-- lineInsideBox :: Line -> (Point, Point) -> [Line]
lineInsideBox box@(boxp00, boxp11) line@(p1, p2) =
  let (inside, outside) = partition (pointInsideBox box) [p1, p2] in
  (inside, outside)
  -- if length pointsInside == 2 then [line] else []
  -- If bot points of line is inside box -> return line as is
  -- otherwise construct new line from intersections
  -- let boxp01 = (fst boxp00, snd boxp11) in
  -- let boxp10 = (fst boxp11, snd boxp00) in
  -- let intersections = fmap (intersection line) [(boxp00, boxp01), (boxp01, boxp11), (boxp11, boxp10), (boxp10, boxp00)] in
  -- intersections


