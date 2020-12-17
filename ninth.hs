
isValid :: [Integer] -> Integer -> Bool
isValid numbers x = x `elem` concat (map (\n -> [y + n | y <- numbers]) numbers)

findInvalid :: [Integer] -> Integer
findInvalid = findInvalid' 25

findInvalid' :: Int -> [Integer] -> Integer
findInvalid' size nums@(_:rest) =
    if isValid (take size nums) (nums !! size) then findInvalid' size rest
    else nums !! size

findSequence :: [Integer] -> Integer -> (Integer, Integer)
findSequence seq@(_:rest) needle
    | res == True = (minimum subsequence, maximum subsequence)
    | res == False = findSequence rest needle 
    where
        (res, count) = checkSequence seq needle
        subsequence = take count seq


checkSequence :: [Integer] -> Integer -> (Bool, Int)
checkSequence sequence n = checkSequence' sequence 0 n 0

checkSequence' :: [Integer] -> Integer -> Integer -> Int -> (Bool, Int)
checkSequence' (x:xs) s n count
    | acc < n = checkSequence' xs acc n (count + 1)
    | acc == n = (True, count)
    | acc > n = (False, count)
    where acc = x + s

main = do
    content <- getContents
    let nums = map read $ lines content
    putStrLn $ show $ uncurry (+) $ findSequence nums $ findInvalid nums