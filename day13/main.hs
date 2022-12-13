import Data.Char
import Data.List.Split

data Packet = DataList [Packet] | Data Int
instance Show Packet where
  show (DataList a) = show a
  show (Data a) = show a

parseDataInt :: String -> (Packet, String)
parseDataInt s = let (i, remaining) = head $ (reads :: ReadS Int) s in (Data i, remaining)

parseDataList :: [Packet] -> String -> ([Packet], String)
parseDataList p (']':xs) = (p, xs)
parseDataList p (',':xs) = parseDataList p xs
parseDataList p s@(x:xs) = let (packet, remaining) = parseDataR s in parseDataList (p ++ [packet]) remaining

parseDataR :: String -> (Packet, String)
parseDataR s@('[':xs) = let (packets, remaining) = parseDataList [] xs in (DataList packets, remaining)
parseDataR s = parseDataInt s

parseData :: String -> Packet
parseData s = fst $ parseDataR s

listPair [a, b] = (a, b)
parseInput input = fmap (listPair . fmap parseData) (splitWhen (== "") (lines input))

compareData :: Packet -> Packet -> Ordering
compareData (Data n) (Data m) = compare n m
compareData (DataList a) (DataList b) =
  let comp = foldl (\r (ae, be) -> if r /= EQ then r else compareData ae be) EQ (zip a b) in
  if comp /= EQ then comp else compare (length a) (length b)
compareData d@(Data n) dataList = compareData (DataList [d]) dataList
compareData dataList d@(Data n)  = compareData dataList (DataList [d])

part1 packetPairs = sum $ fmap fst $ filter (\(_, ord) -> ord == LT) $ zip [1..] $ fmap (uncurry compareData) packetPairs

main = do
  packetPairsE1 <- parseInput <$> readFile "input_e1.txt"
  print $ part1 packetPairsE1
  packetPairs <- parseInput <$> readFile "input.txt"
  print $ part1 packetPairs

