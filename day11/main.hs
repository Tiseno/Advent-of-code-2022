{-# LANGUAGE ScopedTypeVariables #-}

import qualified Data.List as List
import qualified Data.Map  as Map

-- Parsing
data Operation = Square | Double | Add Integer | Mul Integer
  deriving Show

type Id = Integer

data Test = Divisible {
  divisibleBy :: Integer,
  target1 :: Id,
  target2 :: Id
  } deriving Show

type Item = Integer

data Monkey = Monkey {
  monkeyId :: Id,
  items :: [Item],
  operation :: Operation,
  test :: Test,
  inspections :: Integer
  } deriving Show

parseId s = read (init s) :: Integer

parseItems :: String -> [Item]
parseItems s = read ("["++drop 16 s++"]") :: [Integer]

parseOperation :: String -> Operation
parseOperation s = let [op, f2] = drop 4 $ words s in
  if f2 == "old" then if op == "*" then Square else Double
  else if op == "*" then Mul (read f2) else Add (read f2)

parseTest :: String -> String -> String -> Test
parseTest test t f = Divisible (read $ last $ words test) (read $ last $ words t) (read $ last $ words f)

parseMonkeyWords s = let id:items:operation:test:ifTrue:ifFalse:_ = s in
  Monkey (parseId id) (parseItems items) (parseOperation operation) (parseTest test ifTrue ifFalse) 0

splitOnMonkeyR :: ([String], String) -> String -> ([String], String)
splitOnMonkeyR (acc, s) [] = (acc ++ [s], "")
splitOnMonkeyR (acc, s) xs =
  let prefix = "Monkey" in
  if prefix `List.isPrefixOf` xs
  then splitOnMonkeyR (acc ++ [s], "") (drop (length prefix) xs)
  else splitOnMonkeyR (acc, s ++ [head xs]) (tail xs)

splitOnMonkey :: String -> [String]
splitOnMonkey s = drop 1 $ fst $ splitOnMonkeyR ([], "") s

parseMonkeys :: String -> [Monkey]
parseMonkeys = fmap (parseMonkeyWords . fmap (unwords . words) . lines) . splitOnMonkey

-- Business
type MonkeyState = Map.Map Id Monkey

createMonkeyState :: [Monkey] -> MonkeyState
createMonkeyState monkeys = foldl (\b m -> Map.insert (monkeyId m) m b) Map.empty monkeys

inspect :: Item -> Operation -> Item
inspect item Square = item * item
inspect item Double = item + item
inspect item (Add i) = item + i
inspect item (Mul f) = item * f

acquireTarget :: Item -> Test -> Id
acquireTarget item (Divisible divisibleBy target1 target2) = if item `mod` divisibleBy == 0 then target1 else target2

updateMonkey :: Monkey -> MonkeyState -> MonkeyState
updateMonkey monkey ms = Map.insert (monkeyId monkey) monkey ms

throwItem :: Item -> Id -> MonkeyState -> MonkeyState
throwItem item targetId ms = let target = ms Map.! targetId in updateMonkey (target{ items = items target ++ [item] }) ms

inspectAndThrow :: (Item -> Item) -> (Item -> Item) -> Monkey -> MonkeyState -> Item -> MonkeyState
inspectAndThrow afterInspect beforeThrow m ms item =
  let inspected = afterInspect $ inspect item (operation m) in
  let targetId = acquireTarget inspected (test m) in
  let modified = beforeThrow inspected in
  throwItem modified targetId ms

turn :: (Item -> Item) -> (Item -> Item) -> MonkeyState -> Id -> MonkeyState
turn afterInspect beforeThrow ms id =
  let monkey = ms Map.! id in
  let updatedState = foldl (inspectAndThrow beforeThrow afterInspect monkey) ms (items monkey) in
  updateMonkey monkey{ items = [], inspections = inspections monkey + toInteger (length (items monkey)) } updatedState

oneRound :: (Item -> Item) -> (Item -> Item) -> MonkeyState -> MonkeyState
oneRound afterInspect beforeThrow ms = foldl (turn afterInspect beforeThrow) ms (take (length ms) $ iterate (+1) 0 :: [Integer])

runNRounds :: Integer -> (Item -> Item) -> (Item -> Item) -> MonkeyState -> MonkeyState
runNRounds rounds afterInspect beforeThrow ms
  | rounds <= 0 = ms
  | otherwise = runNRounds (rounds - 1) afterInspect beforeThrow (oneRound afterInspect beforeThrow ms)

calculateMonkeyBusiness :: Integer -> (Item -> Item) -> (Item -> Item) -> MonkeyState -> Integer
calculateMonkeyBusiness rounds afterInspect beforeThrow ms =
  product $ take 2 $ reverse $ List.sort $
  fmap inspections $
  Map.elems $ runNRounds rounds afterInspect beforeThrow ms

calmDown :: Item -> Item
calmDown item = item `div` 3

modMod :: [Monkey] -> Item -> Item
modMod monkeys i = i `mod` product (fmap (divisibleBy . test) monkeys)

main = do
  monkeysExample <- parseMonkeys <$> readFile "input_e1.txt"
  monkeys <- parseMonkeys <$> readFile "input.txt"
  putStrLn "Part 1"
  print $ calculateMonkeyBusiness 20 id calmDown (createMonkeyState monkeysExample)
  print $ calculateMonkeyBusiness 20 id calmDown (createMonkeyState monkeys)
  putStrLn "\nPart 2"
  print $ calculateMonkeyBusiness 10000 (modMod monkeysExample) id (createMonkeyState monkeysExample)
  print $ calculateMonkeyBusiness 10000 (modMod monkeys) id (createMonkeyState monkeys)

