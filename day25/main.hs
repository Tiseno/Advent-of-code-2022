import qualified Data.Char as Char

snafuToInt s = fst $ r 0 s
  where
    r i [] = (i, [])
    r i ('=':xs) = r ((i * 5) - 2) xs
    r i ('-':xs) = r ((i * 5) - 1) xs
    r i ('0':xs) = r (i * 5)       xs
    r i ('1':xs) = r ((i * 5) + 1) xs
    r i ('2':xs) = r ((i * 5) + 2) xs

powerSum a b
 | b <  0 = 0
 | otherwise = a^b + powerSum a (b - 1)

snafuValue v n = v * 5^n - 2 * powerSum 5 (n - 1)

firstSnafuPower i (value, power) =
  let (nextValue, nextPower) = if value == 1 then (2, power) else (1, power + 1) in
  if snafuValue nextValue nextPower > i then (value, power) else firstSnafuPower i (nextValue, nextPower)

intToBase5 :: Int -> Int -> [Int]
intToBase5 _ 0 = []
intToBase5 power i = let p = 5 ^ power in
  if i < p then 0 : intToBase5 (power - 1) i else
  i `div` p : intToBase5 (power - 1) (i `mod` p)

base5DigitToSnafuDigit i
  | i == 0 = '='
  | i == 1 = '-'
  | i == 2 = '0'
  | i == 3 = '1'
  | i == 4 = '2'

intToSnafu i = do
  let (firstValue, firstPower) = firstSnafuPower i (1, 0)
  let remainder = i - snafuValue firstValue firstPower
  let base5digits = intToBase5 (firstPower - 1) remainder
  let snafuDigits = Char.chr (firstValue + 48) : fmap base5DigitToSnafuDigit base5digits
  snafuDigits

part1 snafus = intToSnafu $ sum $ fmap snafuToInt snafus

main = do
  snafuNumbers <- lines <$> readFile "input.txt"
  putStrLn $ part1 snafuNumbers

