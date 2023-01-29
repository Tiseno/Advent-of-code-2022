import qualified Data.Map as Map

data Op = Add | Sub | Mul | Div
  deriving Show

operation op = case op of
  Add -> (+)
  Sub -> (-)
  Mul -> (*)
  Div -> div

data Monkey = Calculating Op String String | Value Integer
  deriving Show

parseMonkey s = case words s of
  [name, num] -> (init name, Value (read num))
  [name, name2, "+", name3] -> (init name, Calculating Add name2 name3)
  [name, name2, "-", name3] -> (init name, Calculating Sub name2 name3)
  [name, name2, "*", name3] -> (init name, Calculating Mul name2 name3)
  [name, name2, "/", name3] -> (init name, Calculating Div name2 name3)

parseMonkeys s = Map.fromList (fmap parseMonkey s)

evaluateMonkey :: Map.Map String Monkey -> String -> Integer
evaluateMonkey m s = case m Map.! s of
  Calculating op a b -> (operation op) (evaluateMonkey m a) (evaluateMonkey m b)
  Value n -> n

part1 m = evaluateMonkey m "root"

data Part2Expr =  Unknown | Val Integer | Expr Op Part2Expr Part2Expr | Equation Part2Expr Part2Expr
  deriving Show

createPart2Expr :: Map.Map String Monkey -> String -> Part2Expr
createPart2Expr m "humn" = Unknown
createPart2Expr m s = case m Map.! s of
  Calculating op a b -> let (e1, e2) = (createPart2Expr m a, createPart2Expr m b) in
    if s == "root" then Equation e1 e2 else Expr op e1 e2
  Value n -> Val n

simplifyEquation Unknown e = Equation Unknown e
simplifyEquation e Unknown = Equation Unknown e
simplifyEquation e1@(Val _) e2@(Val _) = Equation e1 e2
simplifyEquation (Val i) (Expr Div e1 e2) = simplifyEquation (eval (Expr Mul (Val i) e2)) e1
simplifyEquation (Val i) (Expr Mul (Val i2) e2) = simplifyEquation (eval (Expr Div (Val i) (Val i2))) e2
simplifyEquation (Val i) (Expr Mul e1 (Val i2)) = simplifyEquation (eval (Expr Div (Val i) (Val i2))) e1
simplifyEquation (Val i) (Expr Add (Val i2) e2) = simplifyEquation (eval (Expr Sub (Val i) (Val i2))) e2
simplifyEquation (Val i) (Expr Add e1 (Val i2)) = simplifyEquation (eval (Expr Sub (Val i) (Val i2))) e1
simplifyEquation (Val i) (Expr Sub (Val i2) e2) = simplifyEquation (eval (Expr Sub (Val i2) (Val i))) e2
simplifyEquation (Val i) (Expr Sub e1 (Val i2)) = simplifyEquation (eval (Expr Add (Val i) (Val i2))) e1
simplifyEquation e1@(Val i) e2@(Equation _ _) = Equation e1 e2
simplifyEquation e1 e2 = simplifyEquation e2 e1

simplify e@(Equation e1 e2) = simplifyEquation e1 e2
simplify e = e

eval (Equation e1 e2) = Equation (eval e1) (eval e2)
eval Unknown = Unknown
eval (Val i) = Val i
eval (Expr op e1 e2) = case (eval e1, eval e2) of
  (Val i1, Val i2) -> Val (operation op i1 i2)
  (Unknown, Val i) -> Expr op Unknown (Val i)
  (Val i, Unknown) -> Expr op (Val i) Unknown
  (Val i, e) -> Expr op (Val i) e
  (e, Val i) -> Expr op e (Val i)

part2 m = simplify $ eval $ createPart2Expr m "root"

main = do
  monkeySet <- parseMonkeys . lines <$> readFile "input.txt"
  print $ part1 monkeySet
  print $ part2 monkeySet
