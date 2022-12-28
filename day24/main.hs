type Point = (Int, Int, Int)
type PrioPoint = (Int, Point)

pHead :: [PrioPoint] -> (Point, [PrioPoint])
pHead (x:xs) = (snd x, xs)

insert :: PrioPoint -> [PrioPoint] -> [PrioPoint]
insert p [] = [p]
insert p (x:xs) = if fst p < fst x then p:x:xs else x : insert p xs

data Matrix a = Matrix [[Char]]
type TimeLine = [Matrix]


--					distance, paths,
data DState = DState (Matrix, Matrix,
part1 = id

main = do
  initialState <- lines <$> readFile "input.txt"
  mapM_ putStrLn initialState
  print $ part1 [initialState]
