import Data.List

diffNext :: [Int] -> [Int]
diffNext (h:next:rest) = next - h : diffNext (next:rest)
diffNext _ = [3]


dynamicOptions :: [Int] -> Int
dynamicOptions list = dynamicOptions' $ zip list (1:(repeat 0))

dynamicOptions' :: [(Int, Int)] -> Int
dynamicOptions' [] = 0
dynamicOptions' [(_, count)] = count
dynamicOptions' ((x, count):rest) = dynamicOptions' (updatedPossible ++ drop 3 rest)
    where
        nextPossible = take 3 rest
        nextOptions = map (+x) [1..3]
        updatedPossible = [if fst possible `elem` nextOptions then (fst possible, snd possible + count) else possible | possible <- nextPossible]



options :: [Int] -> Int
options (x:y:rest)
    | x == 3 || y == 3 = options (y:rest)
    | x + y <= 3 = options (y:rest) + options ((x+y):rest)
options _ = 1



main = do
    content <- getContents
    --putStrLn $ show $ product $ map (length . uncurry filter) $ zip (map (==) [1, 3]) $ repeat $ diffNext $ 0:(sort $ map (\x -> read x:: Int) $ lines content)
    putStrLn $ show $ dynamicOptions $ 0:(sort $ map (\x -> read x:: Int) $ lines content)