import qualified Data.Map.Strict as Map

readCommand :: (Map.Map [String] [Int], [String], [Int]) -> String -> (Map.Map [String] [Int], [String], [Int])
readCommand (m, dirStack, fileSizes) ('$':' ':'c':'d':' ':dirName) =
  let newMap = if not (null fileSizes) then Map.insert dirStack fileSizes m else m in
  let newStack = if dirName == ".." then init dirStack else dirStack ++ [dirName] in
  (newMap, newStack, [])
readCommand (m, dirStack, fileSizes) "$ ls" = (m, dirStack, [])
readCommand (m, dirStack, fileSizes) ('d':_) = (m, dirStack, fileSizes) -- ignore directories
readCommand (m, dirStack, fileSizes) fileStr = (m, dirStack, fileSizes ++ [read (head (words fileStr)) :: Int])

main = do
  l <- fmap lines (readFile "input.txt")
  print (foldl readCommand (Map.empty, [], []) l)

