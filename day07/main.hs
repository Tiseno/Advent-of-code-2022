import qualified Data.Map.Strict as Map
import qualified Data.List as List

data FileTree = Dir (Map.Map String FileTree) | File Int

instance Show FileTree where
  show (File i) = "File " ++ show i
  show (Dir m) = "Dir (" ++ Map.foldrWithKey (\k v s -> "\n" ++ show k ++ ": " ++ show v ++ s) ")" m

-- Takes a file tree and a named file tree and a path to insert it at
insert :: FileTree -> (String, FileTree) -> [String] -> FileTree
insert (File size) _ _ = File size -- inserting something on a file is an invalid operation and should never happen
insert (Dir nodes) (name, node) [] = Dir (Map.insert name node nodes)
insert (Dir nodes) newNode (dirName:path) =
  let dir = Map.findWithDefault (File 0) dirName nodes in
  let newDir = insert dir newNode path in
  Dir (Map.insert dirName newDir nodes)

-- Takes a state (current file tree + current directory) a command string and returns a new state
readCommand :: (FileTree, [String]) -> String -> (FileTree, [String])
readCommand (tree, path) ('$':' ':'c':'d':' ':dirName) =
  if dirName == ".." then (tree, init path) else
  (insert tree (dirName, Dir Map.empty) path, path ++ [dirName])
readCommand s "$ ls" = s
readCommand s ('d':'i':'r':' ':dirName) = s
readCommand (tree, path) fileStr =
  let fileSize = read (head (words fileStr)) :: Int in
  let fileName = head (words fileStr) in
  (insert tree (fileName, File fileSize) path, path)

sizeOf :: FileTree -> Int
sizeOf (File i) = i
sizeOf (Dir nodes) = Map.foldl (\sum node -> sum + sizeOf node) 0 nodes

collectSizes :: [Int] -> FileTree -> [Int]
collectSizes acc (File _) = acc
collectSizes acc (Dir nodes) = sizeOf (Dir nodes) : acc ++ Map.foldl collectSizes [] nodes

solution1 commands = sum $ filter (< 100000) $ collectSizes [] $ fst $ foldl readCommand (Dir Map.empty, []) commands

solution2 commands =
  let fileSystem = fst $ foldl readCommand (Dir Map.empty, []) commands in
  let unused = 70000000 - sizeOf fileSystem in
  let freeSize = 30000000 - unused in
  minimum $ filter (> freeSize) $ collectSizes [] $ fileSystem

main = do
  commands <- lines <$> readFile "input2.txt"
  print $ solution1 commands
  print $ solution2 commands

