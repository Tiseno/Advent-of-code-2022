data Choice = Rock | Paper | Scissors

toChoice "A" = Rock
toChoice "B" = Paper
toChoice "C" = Scissors
toChoice "X" = Rock
toChoice "Y" = Paper
toChoice "Z" = Scissors

toChoices [a, b] = (toChoice a, toChoice b)

data Result = Loss | Draw | Win

result (Rock, Rock)          = Draw
result (Rock, Paper)         = Win
result (Rock, Scissors)      = Loss
result (Paper, Rock)         = Loss
result (Paper, Paper)        = Draw
result (Paper, Scissors)     = Win
result (Scissors, Rock)      = Win
result (Scissors, Paper)     = Loss
result (Scissors, Scissors)  = Draw

choicesToChoiceAndResult (a, b) = (b, result (a, b))

choiceScore Rock     = 1
choiceScore Paper    = 2
choiceScore Scissors = 3

resultScore Win  = 6
resultScore Draw = 3
resultScore Loss = 0

matchScore (b, r) = choiceScore b + resultScore r

toResult "X"  = Loss
toResult "Y"  = Draw
toResult "Z"  = Win

toEnemyChoiceAndResult [a, s]  = (toChoice a, toResult s)

toBestChoice (Rock, Loss)     = Scissors
toBestChoice (Rock, Draw)     = Rock
toBestChoice (Rock, Win)      = Paper
toBestChoice (Paper, Loss)    = Rock
toBestChoice (Paper, Draw)    = Paper
toBestChoice (Paper, Win)     = Scissors
toBestChoice (Scissors, Loss) = Paper
toBestChoice (Scissors, Draw) = Scissors
toBestChoice (Scissors, Win)  = Rock

enemyChoiceAndResultToChoiceAndResult (a, r) = (toBestChoice (a, r),r)

main = do
    greetings <- fmap lines (readFile "input.txt")
    print $ sum $ fmap (matchScore . choicesToChoiceAndResult . toChoices . words) greetings
    print $ sum $ fmap (matchScore . enemyChoiceAndResultToChoiceAndResult . toEnemyChoiceAndResult . words) greetings

