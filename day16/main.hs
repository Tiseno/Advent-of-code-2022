{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Eta reduce" #-}
import qualified Data.Char as Char
import qualified Data.List as List
import qualified Data.Map  as Map
import qualified Data.Set  as Set

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
    tripleExplode n = [(k, i, j) | k <- n, i <- n, j <- n]
    f :: Distances -> (Valve, Valve, Valve) -> Distances
    f g (k, i, j) =
      let existingPath = g Map.! (i, j)
          otherPath = (g Map.! (i, k)) + (g Map.! (k, j))
       in Map.insert (i, j) (min existingPath otherPath) g

type Unvisited = Set.Set Valve

type Memo = Map.Map (Int, Unvisited, Valve) Flow

biggestFlow ::
     Flows -> Distances -> Int -> Unvisited -> Valve -> Memo -> (Flow, Memo)
biggestFlow flows dist steps toVisit current memo =
  biggestFlow' steps toVisit current memo
  where
    biggestFlow' :: Int -> Unvisited -> Valve -> Memo -> (Flow, Memo)
    biggestFlow' stepsLeft _ _ memo
      | stepsLeft <= 0 = (0, memo)
    biggestFlow' stepsLeft unvisited current memo =
      case Map.lookup (stepsLeft, unvisited, current) memo of
        Just f -> (f, memo)
        _ ->
          let (b, m) = foldl (f stepsLeft unvisited current) (0, memo) unvisited
           in (b, Map.insert (stepsLeft, unvisited, current) b m)
    f :: Int -> Unvisited -> Valve -> (Flow, Memo) -> Valve -> (Flow, Memo)
    f stepsLeft unvisited from (prevMax, memo) to =
      let distanceTo = dist Map.! (from, to)
       in if distanceTo > stepsLeft + 1
            then (prevMax, memo)
            else let stepsLeft' = stepsLeft - distanceTo - 1
                     score = flows Map.! to * stepsLeft'
                     (flow, memo') =
                       biggestFlow' stepsLeft' (Set.delete to unvisited) to memo
                     currentMax = score + flow
                  in (max prevMax currentMax, memo')

biggestFlow2 ::
     Flows
  -> Distances
  -> Int
  -> Unvisited
  -> Valve
  -> Memo
  -> Memo
  -> (Flow, Memo, Memo)
biggestFlow2 flows dist steps toVisit current memo ememo =
  biggestFlow' steps toVisit current memo ememo
  where
    biggestFlow' ::
         Int -> Unvisited -> Valve -> Memo -> Memo -> (Flow, Memo, Memo)
    biggestFlow' stepsLeft _ _ memo ememo
      | stepsLeft <= 0 = (0, memo, ememo)
    biggestFlow' stepsLeft unvisited current memo ememo =
      case Map.lookup (stepsLeft, unvisited, current) memo of
        Just f -> (f, memo, ememo)
        _ ->
          let (b, memo', ememo') =
                foldl (f stepsLeft unvisited current) (0, memo, ememo) unvisited
           in (b, Map.insert (stepsLeft, unvisited, current) b memo', ememo')
    f :: Int
      -> Unvisited
      -> Valve
      -> (Flow, Memo, Memo)
      -> Valve
      -> (Flow, Memo, Memo)
    f stepsLeft unvisited from (prevMax, memo, ememo) to =
      let distanceTo = dist Map.! (from, to)
       in if distanceTo > stepsLeft + 1
            then let (eflow, ememo') =
                       biggestFlow flows dist 26 unvisited ('A', 'A') ememo
                  in (max prevMax eflow, memo, ememo')
            else let stepsLeft' = stepsLeft - distanceTo - 1
                     score = flows Map.! to * stepsLeft'
                     toVisit' = Set.delete to unvisited
                     (flow, memo', ememo') =
                       biggestFlow' stepsLeft' toVisit' to memo ememo
                     (eflow, ememo'') =
                       biggestFlow flows dist 26 toVisit' ('A', 'A') ememo'
                     currentMax = score + flow
                     eCurrentMax = score + eflow
                  in (max (max prevMax currentMax) eCurrentMax, memo', ememo'')

part1 input =
  let nodes = parseInput input
      flows = Map.fromList $ fmap (\(v, (fr, _)) -> (v, fr)) nodes
      dist = allDistances nodes
      toVisit =
        Set.fromList $ fmap fst $ filter (\(_, f) -> f > 0) $ Map.toList flows
      (flow, _) = biggestFlow flows dist 30 toVisit ('A', 'A') Map.empty
   in flow

part2 input =
  let nodes = parseInput input
      flows = Map.fromList $ fmap (\(v, (fr, _)) -> (v, fr)) nodes
      dist = allDistances nodes
      toVisit =
        Set.fromList $ fmap fst $ filter (\(_, f) -> f > 0) $ Map.toList flows
      (flow, _, _) =
        biggestFlow2 flows dist 26 toVisit ('A', 'A') Map.empty Map.empty
   in flow

main = do
  exampleInput <- readFile "example.input.txt"
  input <- readFile "input.txt"
  putStrLn "Part 1"
  print $ part1 exampleInput
  print $ part1 input
  putStrLn "Part 2"
  print $ part2 exampleInput
  print $ part2 input
