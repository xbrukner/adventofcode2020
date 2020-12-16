import System.Environment
import Data.List
import Data.Tuple

-- testInput = [1721, 979, 366, 299, 675, 1456]

solve2 :: [Integer] -> Integer
solve2 lst = head [ a * b | a <- lst, b <- lst, a + b == 2020 ]

solve3 :: [Integer] -> Integer
solve3 lst = head [ a * b * c | a <- lst, b <- lst, c <- lst, a + b + c == 2020 ]

solvers :: [[Integer] -> Integer]
--solvers = [\x -> 0, \x -> 1, solve2, solve3]
solvers = map (solveN 2020) [0..]

solveN :: Integer -> Int -> [Integer] -> Integer
solveN expected dimensions = product . head . filter ((== expected) . sum) . generateLists dimensions

{-
generateLists :: Int -> [Integer] -> [[Integer]]
generateLists dimensions = generateLists' dimensions 0

generateLists' :: Int -> Int -> [Integer] -> [[Integer]]
generateLists' dimensions index list
    | index == (length list) ^ dimensions = []
    | otherwise = extractNums index dimensions list : generateLists' dimensions (succ index) list

extractNums :: Int -> Int -> [Integer] -> [Integer]
extractNums index dimensions list = map (list !!) $ take dimensions $ unfoldr (\x -> Just $ swap $ quotRem x size) index
    where size = length list
-}
generateLists :: Int -> [Integer] -> [[Integer]]
generateLists dimensions list = generateLists' dimensions (take dimensions $ repeat 0) (take dimensions $ repeat $ (length list) - 1) list

generateLists' :: Int -> [Int] -> [Int] -> [Integer] -> [[Integer]]
generateLists' dimensions index maxIndex list
    | index == maxIndex = [extractNums index list]
    | otherwise = extractNums index list : generateLists' dimensions (nextIndex size index) maxIndex list
        where size = length list

nextIndex :: Int -> [Int] -> [Int]
nextIndex size = snd . mapAccumL (\add el -> if el + add == size then (1, 0) else (0, el + add)) 1

extractNums :: [Int] -> [Integer] -> [Integer]
extractNums index list = map (list !!) index


extractList :: String -> [Integer]
extractList = map read . words

main = do
    content <- getContents
    args <- getArgs
    putStrLn $ show $ solvers !! read (head args) $ extractList content
