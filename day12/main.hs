import Data.Char
import Data.List
import Data.Matrix

-- Setup
data Point = Point {
  _x :: Int,
  _y :: Int
} deriving Show

elemPoint :: Eq a => a -> [[a]] -> Point
elemPoint a m =
  let (Just x) = findIndex (elem a) m in
  let (Just y) = elemIndex a (m !! x) in
  Point x y

charToHeight :: Char -> Int
charToHeight c = let c2 | c == 'S' = 'a' | c == 'E' = 'z' | otherwise = c in ord c2 - 97

initialDistance :: Int -> Char -> Int
initialDistance e c | c == 'S' = 0 | otherwise = e

-- TODO

main = do
  chars <- lines <$> readFile "input_e1.txt"
  mapM_ putStrLn chars
  let charMatrix = fromLists chars
  let heightMap = fmap charToHeight charMatrix
  let distances = fmap (initialDistance (-1)) charMatrix
  let start = elemPoint 'S' chars
  let end = elemPoint 'E' chars
  print heightMap
  print distances
  print start
  print end

