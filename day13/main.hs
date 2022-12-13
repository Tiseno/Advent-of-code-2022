import Data.Char
import Data.List
import Data.List.Split

data Packet = DataList [Packet] | Data Int

instance Show Packet where
  show (DataList a) = show a
  show (Data a) = show a

instance Eq Packet where
  (==) (Data n) (Data m) = n == m
  (==) (DataList a) (DataList b) = a == b
  (==) _ _ = False

instance Ord Packet where
  compare (Data n) (Data m) = compare n m
  compare (DataList a) (DataList b) =
    let comp = foldl (\r (ae, be) -> if r /= EQ then r else compare ae be) EQ (zip a b) in
    if comp /= EQ then comp else compare (length a) (length b)
  compare d@(Data n) dataList = compare (DataList [d]) dataList
  compare dataList d@(Data n)  = compare dataList (DataList [d])

parseData :: String -> (Packet, String)
parseData s = let (i, remaining) = head $ (reads :: ReadS Int) s in (Data i, remaining)

parseDataList :: [Packet] -> String -> ([Packet], String)
parseDataList p (']':xs) = (p, xs)
parseDataList p (',':xs) = parseDataList p xs
parseDataList p s@(x:xs) = let (packet, remaining) = parsePacketR s in parseDataList (p ++ [packet]) remaining

parsePacketR :: String -> (Packet, String)
parsePacketR s@('[':xs) = let (packets, remaining) = parseDataList [] xs in (DataList packets, remaining)
parsePacketR s = parseData s

parsePacket :: String -> Packet
parsePacket s = fst $ parsePacketR s

parsePacketPairs input = fmap ((\[a, b] -> (a, b)) . fmap parsePacket) (splitWhen (== "") (lines input))
parsePackets input = parsePacket <$> filter (/="") (lines input)

part1 packetPairs = sum $ fmap fst $ filter (\(_, ord) -> ord == LT) $ zip [1..] $ fmap (uncurry compare) packetPairs
part2 packets = let extraPackets = [parsePacket "[[2]]", parsePacket "[[6]]]"] in
  product $ fmap fst $ filter (\(_, p) -> p `elem` extraPackets) $ zip ([1..]::[Int]) $ sort (packets ++ extraPackets)

main = do
  inputE1 <- readFile "input_e1.txt"
  input <- readFile "input.txt"
  print $ part1 $ parsePacketPairs inputE1
  print $ part1 $ parsePacketPairs input
  print $ part2 $ parsePackets inputE1
  print $ part2 $ parsePackets input

