import qualified Debug.Trace     as Trace
import qualified Data.MultiMap   as MultiMap
import qualified Data.MultiSet   as MultiSet
import qualified Data.Set        as Set
import qualified Data.List.Index as Index
import Data.Maybe

debug n = Trace.traceShow n n

type Point = (Int, Int)

type Blizzards = MultiMap.MultiMap Point Char

nextBlizzard :: Point -> Char -> Point
nextBlizzard (x, y) c
  | c == '#' = (x, y)
  | c == 'v' = (x, y + 1)
  | c == '<' = (x - 1, y)
  | c == '^' = (x, y - 1)
  | c == '>' = (x + 1, y)

wrapBlizzard (xmax, ymax) c (x, y)
  | c == '#' = (x, y)
  | x == 0 = (xmax - 2, y)
  | y == 0 = (x, ymax - 2)
  | x == xmax - 1 = (1, y)
  | y == ymax - 1 = (x, 1)
  | otherwise = (x, y)

moveBlizzard wh p c = wrapBlizzard wh c $ nextBlizzard p c

moveBlizzards :: Point -> Blizzards -> Blizzards
moveBlizzards wh = MultiMap.foldlWithKey (\newBlizzards p b -> MultiMap.insert (moveBlizzard wh p b) b newBlizzards) MultiMap.empty

positivePoint p = fst p >= 0 && snd p >= 0

possibleMoves :: Point -> [Point]
possibleMoves (x, y) = [(x, y), (x + 1, y), (x, y + 1), (x - 1, y), (x, y - 1)]

legalMoves :: Blizzards -> Point -> [Point]
legalMoves blizzards p = filter (MultiMap.notMember blizzards) $ filter positivePoint $ possibleMoves p

movePositions :: Blizzards -> Set.Set Point -> Set.Set Point
movePositions blizzards positions = Set.fromList $ concatMap (legalMoves blizzards) positions

bfs :: Point -> Point -> Int -> Blizzards -> Set.Set Point -> (Int, Blizzards)
bfs end wh n blizzards positions =
  if end `Set.member` positions || null positions
  then (n, blizzards) else
  let newBlizzards = moveBlizzards wh blizzards in
  let newPositions = movePositions newBlizzards positions in
  bfs end wh (n + 1) newBlizzards newPositions

findEnd :: [[Char]] -> Point
findEnd m = (length (last m) - 2, length m - 1)

findWH :: [[Char]] -> Point
findWH m = (length $ head m, length m)

pointMap :: (Point -> a -> b) -> [[a]] -> [[b]]
pointMap fn = Index.imap (\y row -> Index.imap (\x e -> fn (x, y) e) row)

isBlizzard c = c == 'v' || c == '<' || c == '^' || c == '>' || c == '#'

createBlizzards :: [[Char]] -> Blizzards
createBlizzards m = MultiMap.fromList $ catMaybes $ concat $ pointMap (\p e -> if isBlizzard e then Just (p, e) else Nothing) m

part1 m = fst $ bfs (findEnd m) (findWH m) 0 (createBlizzards m) (Set.fromList [(1,0)])

part2 m =
  let start = (1,0) in
  let end = findEnd m in
  let wh = findWH m in
  let b0 = createBlizzards m in
  let (n1, b1) = bfs end wh 0 b0 (Set.fromList [start]) in
  let (n2, b2) = bfs start wh n1 b1 (Set.fromList [end]) in
  let (n3, _) = bfs end wh n2 b2 (Set.fromList [start]) in
  n3

main = do
  basin <- lines <$> readFile "input.txt"
  mapM_ putStrLn basin
  print $ part1 basin
  print $ part2 basin

