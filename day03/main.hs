import qualified Data.Char    as Char
import qualified Data.Set     as Set
import qualified Data.List    as List

valueOfChar c = let ord = Char.ord c in if ord > 90 then ord - 96 else ord - 38

findCommonText (x:xs) = foldl List.intersect x xs

chunksOf _ [] = []
chunksOf n xs = let (ys, zs) = splitAt n xs in ys : chunksOf n zs

chunksOfHalf a = chunksOf (length a `div` 2) a

part1 p = sum (fmap (sum . Set.map valueOfChar . Set.fromList . findCommonText . chunksOfHalf) p)
part2 p = sum (fmap (sum . Set.map valueOfChar . Set.fromList . findCommonText) (chunksOf 3 p))

main = do
    strings1 <- fmap lines (readFile "input.txt")
    strings2 <- fmap lines (readFile "input2.txt")
    print $ part1 strings1
    print $ part1 strings2
    print $ part2 strings1
    print $ part2 strings2
