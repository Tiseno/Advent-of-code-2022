import qualified Data.Set       as Set
import qualified Data.MultiSet  as MultiSet
import qualified Data.Map       as Map
import qualified GHC.Utils.Misc as Misc
import qualified GHC.Data.Maybe as Maybe

type Point = (Int, Int)
type Elf = (Int, Int)
type Elfs = Set.Set Elf

parseElfR :: (Point, Elfs) -> Char -> (Point, Elfs)
parseElfR ((x, y), elfs) '.' = ((x + 1, y), elfs)
parseElfR (p@(x, y), elfs) '#' = ((x + 1, y), Set.insert p elfs)

parseElfsR :: (Int, Elfs) -> String -> (Int, Elfs)
parseElfsR (y, elfs) s = (y + 1, snd $ foldl parseElfR ((0, y), elfs) s)

parseElfs :: [String] -> Elfs
parseElfs s = snd $ foldl parseElfsR (0, Set.empty) s

data Direction = North | South | West | East
  deriving Show
type Directions = [Direction]
data Axis = Vertical | Horizontal

considerPoint :: Elfs -> Axis -> Point -> Bool
considerPoint elfs Horizontal p@(x, y) = p `Set.notMember` elfs && (x - 1, y) `Set.notMember` elfs && (x + 1, y) `Set.notMember` elfs
considerPoint elfs Vertical p@(x, y) = p `Set.notMember` elfs && (x, y - 1) `Set.notMember` elfs && (x, y + 1) `Set.notMember` elfs

considerDirection :: Elfs -> Elf -> Direction -> Maybe Point
considerDirection elfs (x, y) direction =
  let (axis, p) = case direction of
        North -> (Horizontal, (x, y - 1))
        South -> (Horizontal, (x, y + 1))
        West ->  (Vertical, (x - 1, y))
        East ->  (Vertical, (x + 1, y)) in
  if considerPoint elfs axis p then Just p else Nothing

elfAlone :: Elfs -> Elf -> Bool
elfAlone elfs (x, y) = all (`Set.notMember` elfs) [(x + 1, y), (x - 1, y), (x, y + 1), (x, y - 1), (x + 1, y + 1), (x + 1, y - 1), (x - 1, y + 1), (x - 1, y - 1)]

considerMove :: (Directions, Elfs) -> Elf -> Point
considerMove (directions, elfs) elf
  | elfAlone elfs elf = elf
  | otherwise = Maybe.fromMaybe elf (Maybe.firstJusts $ fmap (considerDirection elfs elf) directions)

type Moves = Map.Map Elf Point
firstHalf :: (Directions, Elfs) -> Elfs -> Moves
firstHalf de = Map.fromSet (considerMove de)

moveElf :: MultiSet.MultiSet Point -> Elfs -> Elf -> Point -> Elfs
moveElf proposed elfs elf move
  | MultiSet.occur move proposed > 1 = Set.insert elf elfs
  | otherwise = Set.insert move elfs

secondHalf :: Moves -> Elfs
secondHalf moves = Map.foldlWithKey (moveElf (MultiSet.fromList (Map.elems moves))) Set.empty moves

elfRound (elfs, directions) = do
  let moves = firstHalf (directions, elfs) elfs
  let movedElfs = secondHalf moves
  let newDirections = tail directions ++ [head directions]
  (movedElfs, newDirections)

elfRounds n elfs directions = Misc.nTimes n elfRound (elfs, directions)

big = 10000000000000

findArea elfs = let ((xmin, ymin), (xmax, ymax)) = foldl (\((xmin, ymin), (xmax, ymax)) (x, y) -> ((min xmin x, min ymin y), (max xmax x, max ymax y))) ((big, big), (-big, -big)) elfs in
  (xmax + 1 - xmin) * (ymax + 1 - ymin)

part1 :: Int -> Elfs -> Int
part1 n elfs = do
  let (resultingElfs, _) = elfRounds n elfs [North, South, West, East]
  let area = findArea resultingElfs
  area - length elfs

elfRoundsUntilStable n (elfs, directions) =
  let (newElfs, newDirections) = elfRound (elfs, directions) in
  if elfs == newElfs then n else elfRoundsUntilStable (n + 1) (newElfs, newDirections)

part2 elfs = elfRoundsUntilStable 1 (elfs, [North, South, West, East])

main = do
  elfState1 <- parseElfs . lines <$> readFile "input_e1.txt"
  elfState2 <- parseElfs . lines <$> readFile "input_e2.txt"
  elfState <- parseElfs . lines <$> readFile "input.txt"
  print $ part1 10 elfState1
  print $ part1 10 elfState2
  print $ part1 10 elfState
  print $ part2 elfState1
  print $ part2 elfState2
  print $ part2 elfState

