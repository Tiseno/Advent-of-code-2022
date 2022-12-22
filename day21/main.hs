import qualified Data.Map as Map

data Monkey = Adding String String | Subbing String String | Multing String String | Divving String String | Value Int
  deriving Show

parseMonkey s = case words s of
  [name, num] -> (init name, Value (read num))
  [name, name2, "+", name3] -> (init name, Adding name2 name3)
  [name, name2, "-", name3] -> (init name, Subbing name2 name3)
  [name, name2, "*", name3] -> (init name, Multing name2 name3)
  [name, name2, "/", name3] -> (init name, Divving name2 name3)

parseMonkeys s = Map.fromList (fmap parseMonkey s)

eval :: Map.Map String Monkey -> String -> Int
eval m s = case m Map.! s of
  Adding a b -> eval m a + eval m b
  Subbing a b -> eval m a - eval m b
  Multing a b -> eval m a * eval m b
  Divving a b -> eval m a `div` eval m b
  Value n -> n

part1 m = eval m "root"

main = do
  monkeySet <- parseMonkeys . lines <$> readFile "input.txt"
  print $ part1 monkeySet
