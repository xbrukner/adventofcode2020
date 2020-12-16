isTree :: Char -> Bool
isTree '#' = True
isTree _ = False

traverseMap :: (Char -> Bool) -> (Int, Int) -> [String] -> [Bool]
traverseMap fn movement = traverseWithOffset fn movement 0

traverseWithOffset :: (Char -> Bool) -> (Int, Int) -> Int -> [String] -> [Bool]
traverseWithOffset fn movement@(right, down) offset (line:rest) = current : (traverseWithOffset fn movement (offset + right) $ drop (down - 1) rest)
    where current = fn $ line !! (rem offset $ length line)
traverseWithOffset _ _ _ [] = []

main = do
    content <- getContents
    --putStrLn $ show $ product 
    putStrLn $ show $ product $ map (length . filter (== True) . uncurry (traverseMap isTree)) $ zip config (repeat $ lines content)
    --putStrLn $ show $ product $ map (map (length . filter (== True) . traverseMap isTree) config) (repeat $ lines content)
    where config = [(1, 1), (3, 1), (5, 1), (7, 1), (1, 2)]
    --map (map (\c l -> length $ filter (== True) $ traverseMap isTree c l) config) (lines content)
    