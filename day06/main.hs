slidingR :: Int -> String -> String -> Int
slidingR i [] _ = i
slidingR i (x:xs) [_,b,c,d] = if b /= c && b /= d && b /= x && c /= d && c /= x && d /= x then i else slidingR (i+1) xs [b,c,d,x]
slidingR i (x:xs) window = slidingR (i+1) xs (window ++ [x])

sliding s = slidingR 1 s []

main = do
    input <- fmap lines (readFile "input2.txt")
    print [7, 5, 6, 10, 11]
    print $ fmap sliding input

