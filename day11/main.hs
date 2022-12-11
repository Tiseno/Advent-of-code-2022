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

parseItems :: String -> [Integer]
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

calmDown calmFactor item = item `div` calmFactor

acquireTarget :: Item -> Test -> Id
acquireTarget item (Divisible divisibleBy target1 target2) = if item `mod` divisibleBy == 0 then target1 else target2

updateMonkey :: Monkey -> MonkeyState -> MonkeyState
updateMonkey monkey ms = Map.insert (monkeyId monkey) monkey ms

throwItem :: Item -> Id -> MonkeyState -> MonkeyState
throwItem item targetId ms = let target = ms Map.! targetId in updateMonkey (target{ items = items target ++ [item] }) ms

inspectAndThrow :: Integer -> Monkey -> MonkeyState -> Item -> MonkeyState
inspectAndThrow calmFactor m ms item =
  let inspected = calmDown calmFactor $ inspect item (operation m) in
  let targetId = acquireTarget inspected (test m) in
  throwItem inspected targetId ms

turn :: Integer -> MonkeyState -> Id -> MonkeyState
turn calmFactor ms id =
  let monkey = ms Map.! id in
  let updatedState = foldl (inspectAndThrow calmFactor monkey) ms (items monkey) in
  updateMonkey monkey{ items = [], inspections = inspections monkey + toInteger (length (items monkey)) } updatedState

oneRound :: MonkeyState -> Integer -> MonkeyState
oneRound ms calmFactor = foldl (turn calmFactor) ms (take (length ms) $ iterate (+1) 0 :: [Integer])

runNRounds :: Integer -> MonkeyState -> Integer -> MonkeyState
runNRounds rounds ms calmFactor
  | rounds <= 0 = ms
  | otherwise = runNRounds (rounds - 1) (oneRound ms calmFactor) calmFactor

calculateMonkeyBusiness rounds calmFactor ms =
  product $ take 2 $ reverse $ List.sort $
  fmap inspections $
  Map.elems $ runNRounds rounds ms calmFactor

main = do
  monkeysExample <- parseMonkeys <$> readFile "input_e1.txt"
  monkeys <- parseMonkeys <$> readFile "input.txt"
  putStrLn "Part 1"
  print $ calculateMonkeyBusiness 20 3 (createMonkeyState monkeysExample)
  print $ calculateMonkeyBusiness 20 3 (createMonkeyState monkeys)
  putStrLn "\nPart 2" -- Crashes at ~1000 rounds"
  print $ calculateMonkeyBusiness 100 1 (createMonkeyState monkeys)

