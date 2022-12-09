import qualified Debug.Trace as Trace
import qualified Data.Set as Set

-- Nice trick!
debug n = Trace.traceShow n n

data Direction = U | D | L | R
  deriving (Read, Show)

explodeCommand [d, n] = replicate (read n) (read d)

parseMoves :: [String] -> [Direction]
parseMoves moves = concatMap explodeCommand $ fmap words moves

type Point = (Int, Int)
type Rope = [Point]

moveHead (x, y) U = (x + 1, y)
moveHead (x, y) D = (x - 1, y)
moveHead (x, y) L = (x, y - 1)
moveHead (x, y) R = (x, y + 1)

follow :: Point -> Point -> Point
follow (hx, hy) (xt, yt)
  | abs (xt - hx) < 2 && abs (yt - hy) < 2 = (xt, yt)
  | otherwise = (xt + if hx > xt then 1 else if hx < xt then -1 else 0, yt + if hy > yt then 1 else if hy < yt then -1 else 0)

buildTail :: Rope -> Point -> Rope
buildTail rope tailp = rope++[follow (last rope) tailp]

moveRope (head:tail) direction = foldl buildTail [moveHead head direction] tail

type Visited = Set.Set Point

moveRopeAndVisit :: (Visited, Rope) -> Direction -> (Visited, Rope)
moveRopeAndVisit (visited, rope) direction =
  let newRope = moveRope rope direction in
  (Set.insert (last newRope) visited, newRope)

simulateRope n moves = foldl moveRopeAndVisit (Set.empty, replicate n (0,0)) moves

solution1 moves = length $ fst $ simulateRope 2 moves
solution2 moves = length $ fst $ simulateRope 10 moves

main = do
  moves <- fmap (parseMoves . lines) (readFile "input.txt")
  print $ solution1 moves
  print $ solution2 moves

