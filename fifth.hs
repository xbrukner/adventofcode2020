import Data.List

decodeStr :: String -> String -> Int
decodeStr keys str = sum $ map (uncurry (*)) (zip indexes (iterate (*2) 1)) 
    where indexes = map (\x -> maybe 0 id (lookup x (zip keys [0..]))) (reverse str)

decodeSet :: String -> [Int]
decodeSet = map (uncurry decodeStr) . zip ["FB", "LR"] . (\(x,y) -> [x,y]) . splitAt 7

getSeatId :: [Int] -> Int
getSeatId [row, column] = row * 8 + column
getSeatId _ = error "Invalid set"

main = do
    content <- getContents
    let passes = sort $ map (getSeatId . decodeSet) $ lines content
        allPasses = [head passes .. last passes]
    putStrLn $ show $ allPasses \\ passes