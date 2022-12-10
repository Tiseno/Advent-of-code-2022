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

runCycles :: Int -> Maybe (Int, Instruction) -> (Int, Int) -> [Instruction] -> (Int, Int)
runCycles 0 _ (d, x) _ = (d, x)
runCycles _ Nothing (d, x) [] = (d, x)
runCycles counter Nothing (d, x) (instruction:instructions) = runCycles counter (Just (executionTime instruction, instruction)) (d, x) instructions
runCycles counter (Just (0, instruction)) (_, x) instructions =
  runCycles (counter - 1) Nothing (x, runInstruction x instruction) instructions
runCycles counter (Just (ic, instruction)) (_, x) instructions =
  runCycles (counter - 1) (Just (ic - 1, instruction)) (x, x) instructions

signalStrengthAtCycle instructions n = (*n) $ fst (runCycles n Nothing (1, 1) instructions)

sumOfSixSignalStrengths instructions = sum $ fmap (signalStrengthAtCycle instructions) [20, 60, 100, 140, 180, 220]

main = do
  instructions <- fmap (fmap parseInstruction . lines) (readFile "input.txt")
  print $ sumOfSixSignalStrengths instructions

