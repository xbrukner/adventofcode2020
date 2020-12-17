data Instruction = Nop | Acc | Jmp deriving (Enum, Eq, Show)

executeInstruction :: (Instruction, Int) -> (Int, Int) -> (Int, Int)
executeInstruction (Nop, _) (acc, pos) = (acc, pos + 1)
executeInstruction (Acc, x) (acc, pos) = (acc + x, pos + 1) 
executeInstruction (Jmp, x) (acc, pos) = (acc, pos + x)

parseLine :: String -> (Instruction, Int)
parseLine = parseLine' . splitAt 5

parseLine' :: (String, String) -> (Instruction, Int)
parseLine' ("nop -",_) = (Nop, 0)
parseLine' ("nop +",_) = (Nop, 0)
parseLine' ("acc +",x) = (Acc, read x)
parseLine' ("acc -",x) = (Acc, -read x)
parseLine' ("jmp +",x) = (Jmp, read x)
parseLine' ("jmp -",x) = (Jmp, -read x)
parseLine' x = error ("Unknown instruction" ++ show x)

execute :: [(Instruction, Int)] -> (Int, Int)
execute instructions = execute' instructions (0, 0) []

execute' :: [(Instruction, Int)] -> (Int, Int) -> [Int] -> (Int, Int)
execute' instructions state@(_, pos) visited
    | pos `elem` visited = state
    | pos == length instructions = state
    | otherwise          = execute' instructions (executeInstruction (instructions !! pos) state) (pos:visited)

fixInstructions :: [(Instruction, Int)] -> [[(Instruction, Int)]]
fixInstructions instructions = fixInstructions' instructions 0

fixInstructions' :: [(Instruction, Int)] -> Int -> [[(Instruction, Int)]]
fixInstructions' instructions pos
    | pos == length instructions = []
    | instruction == Acc = next
    | instruction == Jmp = (before ++ [(Nop, snd (instructions !! pos))] ++ after) : next
    | instruction == Nop = (before ++ [(Jmp, snd (instructions !! pos))] ++ after) : next
    | otherwise = error "Invalid state"
    where
        instruction = fst (instructions !! pos)
        before = take pos instructions
        after = drop (pos + 1) instructions
        next = fixInstructions' instructions (pos + 1)

main = do
    content <- getContents
    let instructions = map parseLine $ lines content
    putStrLn $ show $ fst $ head $ filter (\x -> snd x == length instructions) $ map execute $ fixInstructions instructions