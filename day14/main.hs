import qualified Data.Map as Map
import qualified Data.List.Split as Split

rock = '#'
sand = 'o'

type Point = (Int, Int)
type Path = [Point]

parsePoints :: String -> [Point]
parsePoints s = fmap (read :: String -> Point) $ fmap (\p -> "("++p++")") $ Split.splitOn " -> " s

range a b
  | a < b = [a..b]
  | otherwise = [b..a]

expandPath :: [Point] -> Path
expandPath (a:b:xs)
  | fst a == fst b = fmap (fst a,) (range (snd a) (snd b)) ++ expandPath (b:xs)
  | otherwise = fmap (,snd a) (range (fst a) (fst b)) ++ expandPath (b:xs)
expandPath _ = []

parsePath :: String -> Path
parsePath s = expandPath $ parsePoints s

parsePaths = fmap parsePath

type Cave = Map.Map Point Char

insertBlock :: Char -> Cave -> Point -> Cave
insertBlock char cave point = Map.insert point char cave

insertRock = insertBlock rock

insertPath :: Cave -> Path -> Cave
insertPath = foldl insertRock

createCave :: [Path] -> Cave
createCave = foldl insertPath Map.empty

insertSand = insertBlock sand

emptySpaceUnderneath :: Point -> Cave -> Maybe Point
emptySpaceUnderneath (x, y) cave =
  let under = (x, y + 1) in
  let left = (x - 1, y + 1) in
  let right = (x + 1, y + 1) in
  if Map.notMember under cave then Just under else
  if Map.notMember left cave then Just left else
  if Map.notMember right cave then Just right else
  Nothing

dropSandR :: Int -> Point -> Cave -> Cave
dropSandR i p cave
  | i > 0 = case emptySpaceUnderneath p cave of
    Just e -> dropSandR (i - 1) e cave
    Nothing -> insertSand cave p
  | otherwise = cave

dropSand :: Cave -> Cave
dropSand = dropSandR 10000 (500, 0)

dropSandN :: Int -> Cave -> Cave
dropSandN n cave
  | n <= 0 = cave
  | otherwise = dropSandN (n - 1) (dropSand cave)

addSandUntilAbyssFlow :: Cave -> Cave
addSandUntilAbyssFlow cave = let newCave = dropSand cave in
  if length newCave == length cave then cave else addSandUntilAbyssFlow newCave

getSandPoints cave = fmap fst $ filter (\l -> snd l == sand) $ Map.assocs cave

countSand :: Cave -> Int
countSand cave = length $ filter (== sand) $ Map.elems cave

part1 input = do
  let paths = parsePaths input
  let cave = createCave paths
  countSand $ addSandUntilAbyssFlow cave

main = do
  inputE1 <- lines <$> readFile "input_e1.txt"
  input <- lines <$> readFile "input.txt"
  print $ part1 inputE1
  print $ part1 input

