data Instruction = Noop | Addx Int

parseInstruction :: String -> Instruction
parseInstruction "noop" = Noop
parseInstruction instr = Addx $ read $ last $ words instr

executionTime :: Instruction -> Int
executionTime Noop = 0
executionTime (Addx _) = 1

runInstruction Noop register = register
runInstruction (Addx i) register = register + i

drawPixel image x = image ++ [if 2 > abs (length image `mod` 40 - x) then '#' else ' ']

type Counter = Int
type CRTState = (String, Int)
type CpuState = (Counter, Instruction, [Instruction])

runCycles :: Counter -> CRTState -> CpuState -> CRTState
runCycles 0 state (_, _, _) = state
runCycles _ state (_, _, [])  = state
runCycles c (image, x) (0, instr, instructions)  =
  let (next:remaining) = if null instructions then [Noop] else instructions in
  let result = runInstruction instr x in
  runCycles (c - 1) (drawPixel image result, result) (executionTime next, next, remaining)
runCycles c (image, x) (ic, instr, instructions)  =
  runCycles (c - 1) (drawPixel image x, x) (ic - 1, instr, instructions)

-- Part 1
signalStrengthAtCycle instructions n = (*n) $ snd (runCycles n ("", 1) (0, Noop, instructions))
sumOfSixSignalStrengths instructions = sum $ fmap (signalStrengthAtCycle instructions) [20, 60, 100, 140, 180, 220]

-- Part 2
createImage instructions = fst $ runCycles (-1) ("", 1) (0, Noop, instructions)
chunksOf _ [] = []
chunksOf n xs = let (ys, zs) = splitAt n xs in ys : chunksOf n zs
drawImage image = mapM_ putStrLn (chunksOf 40 image)

main = do
  instructions_e2 <- fmap (fmap parseInstruction . lines) (readFile "input_e2.txt")
  print $ sumOfSixSignalStrengths instructions_e2
  instructions <- fmap (fmap parseInstruction . lines) (readFile "input.txt")
  print $ sumOfSixSignalStrengths instructions
  drawImage $ createImage instructions

