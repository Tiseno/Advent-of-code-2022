{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Eta reduce" #-}
import qualified Data.Array  as Array
import qualified Data.Char   as Char
import qualified Data.List   as List
import qualified Data.Map    as Map
import qualified Debug.Trace as Debug

type Valve = (Char, Char)

type Flow = Int

type Node = (Valve, (Flow, [Valve]))

type Flows = Map.Map Valve Flow

type Distance = Int

type Distances = Map.Map (Valve, Valve) Distance

parseInput :: String -> [Node]
parseInput input = foldl f [] $ lines input
  where
    f :: [Node] -> String -> [Node]
    f nodes line =
      let (_:name:_:_:fr:_:_:_:_:e) = words line
          flowRate :: Int = read $ takeWhile Char.isDigit (drop 5 fr)
          edges = fmap (toValve . filter (/= ',')) e
       in (toValve name, (flowRate, edges)) : nodes
    toValve [a, b] = (a, b)

allDistances :: [Node] -> Distances
allDistances nodes = foldl f init $ tripleExplode $ List.nub $ fmap fst nodes
  where
    init :: Distances
    init =
      let self = Map.fromList $ fmap ((, 0) . (\x -> (x, x)) . fst) nodes
          connections =
            Map.fromList
              ((, 1) <$> concatMap (\(from, (_, to)) -> fmap (from, ) to) nodes)
          all =
            Map.fromList
              ((, 9999999) <$>
               concatMap (\(n, _) -> fmap ((n, ) . fst) nodes) nodes)
       in self `Map.union` connections `Map.union` all
    tripleExplode :: [Valve] -> [(Valve, Valve, Valve)]
    tripleExplode names = do
      name1 <- names
      name2 <- names
      name3 <- names
      pure (name1, name2, name3)
    f :: Distances -> (Valve, Valve, Valve) -> Distances
    f g (k, i, j) =
      let existingPath = g Map.! (i, j)
          otherPath = (g Map.! (i, k)) + (g Map.! (k, j))
       in Map.insert (i, j) (min existingPath otherPath) g

biggestFlow :: Flows -> Distances -> Int -> Flow
biggestFlow flows dist steps = biggestScoreFrom toVisit steps ('A', 'A')
  where
    toVisit :: [Valve]
    toVisit = fmap fst $ filter (\(_, f) -> f > 0) $ Map.toList flows
    biggestScoreFrom :: [Valve] -> Int -> Valve -> Int
    biggestScoreFrom _ stepsLeft _
      | stepsLeft <= 0 = 0
    biggestScoreFrom unvisited stepsLeft current =
      foldl (f unvisited stepsLeft current) 0 unvisited
    f :: [Valve] -> Int -> Valve -> Flow -> Valve -> Flow
    f unvisited stepsLeft from prevMax to =
      let distanceTo = dist Map.! (from, to)
       in if distanceTo > stepsLeft + 1
            then prevMax
            else let stepsLeft' = stepsLeft - distanceTo - 1
                     score = flows Map.! to * stepsLeft'
                     currentMax =
                       score +
                       biggestScoreFrom (List.delete to unvisited) stepsLeft' to
                  in max prevMax currentMax

part1 input =
  let nodes = parseInput input
      flows = Map.fromList $ fmap (\(v, (fr, _)) -> (v, fr)) nodes
      dist = allDistances nodes
   in biggestFlow flows dist 30

main = do
  example <- readFile "example.input.txt"
  input <- readFile "input.txt"
  print $ part1 example
  print $ part1 input
