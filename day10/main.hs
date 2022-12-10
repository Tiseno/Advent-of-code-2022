import Data.Maybe

data Instruction = Noop | Addx Int
  deriving Show

parseInstruction :: String -> Instruction
parseInstruction "noop" = Noop
parseInstruction instr = Addx (read (last (words instr)))

executionTime :: Instruction -> Int
executionTime (Addx _) = 1
executionTime Noop = 0

runInstruction register Noop = register
runInstruction register (Addx i) = register + i

drawPixel image x = let drawX = length image `mod` 40 in image ++ [if x < drawX + 2 && x > drawX - 2 then '#' else ' ']

runCycles :: Int -> Maybe (Int, Instruction) -> (String, Int, Int) -> [Instruction] -> (String, Int, Int)
runCycles 0 _ (s, d, x) _ = (s, d, x)
runCycles _ Nothing (s, d, x) [] = (s, d, x)
runCycles counter Nothing (s, d, x) (instruction:instructions) = runCycles counter (Just (executionTime instruction, instruction)) (s, d, x) instructions
runCycles counter (Just (0, instruction)) (s, _, x) instructions =
  runCycles (counter - 1) Nothing (drawPixel s x, x, runInstruction x instruction) instructions
runCycles counter (Just (ic, instruction)) (s, _, x) instructions =
  runCycles (counter - 1) (Just (ic - 1, instruction)) (drawPixel s x, x, x) instructions

fst3 (a, _, _) = a
snd3 (_, a, _) = a

signalStrengthAtCycle instructions n = (*n) $ snd3 (runCycles n Nothing ("", 1, 1) instructions)

sumOfSixSignalStrengths instructions = sum $ fmap (signalStrengthAtCycle instructions) [20, 60, 100, 140, 180, 220]

createImage instructions = fst3 (runCycles 300 Nothing ("", 1, 1) instructions)

chunksOf _ [] = []
chunksOf n xs = let (ys, zs) = splitAt n xs in ys : chunksOf n zs

drawImage image = mapM_ putStrLn (chunksOf 40 image)

main = do
  instructions <- fmap (fmap parseInstruction . lines) (readFile "input.txt")
  print $ sumOfSixSignalStrengths instructions
  drawImage $ createImage instructions

