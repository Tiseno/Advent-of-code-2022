data Instruction = Noop | Addx Int
  deriving Show

parseInstruction :: String -> Instruction
parseInstruction "noop" = Noop
parseInstruction instr = Addx (read (last (words instr)))

valueOfInstruction Noop = 0
valueOfInstruction (Addx i) = i

instructionAtIndex instructions index =
  if index < 0 || index >= length instructions then Noop else instructions !! index

registerAtTime :: Int -> [Instruction] -> Int
registerAtTime time instructions
  | time <= 0 = 1
  | otherwise = registerAtTime (time - 1) instructions + valueOfInstruction (instructionAtIndex instructions (time - 3))

-- Misunderstood the problem completely, thought that the instructions would run in parallel
main = do
  instructions <- fmap (fmap parseInstruction . lines) (readFile "input_e1.txt")
  print $ registerAtTime 5 instructions

