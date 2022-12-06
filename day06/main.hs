hasDoubles :: String -> Bool
hasDoubles [] = False
hasDoubles (x:xs) = elem x xs || hasDoubles xs

slidingR :: Int -> Int -> String -> String -> Int
slidingR windowSize i [] _ = 0
slidingR windowSize i (x : xs) window
    | length (window ++ [x]) < windowSize
    = slidingR windowSize (i + 1) xs (window ++ [x])
    | hasDoubles (window ++ [x])
    = slidingR windowSize (i + 1) xs (tail window ++ [x])
    | otherwise = i

solution1 s = slidingR 4 1 s []
solution2 s = slidingR 14 1 s []

main = do
    input <- fmap lines (readFile "input2.txt")
    print [7, 5, 6, 10, 11]
    print $ fmap solution1 input
    print [19, 23, 23, 29, 26]
    print $ fmap solution2 input

