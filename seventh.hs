import Data.List
import qualified Data.Map.Lazy as Map

-- https://stackoverflow.com/a/4978733
separateBy :: Eq a => a -> [a] -> [[a]]
separateBy chr = unfoldr sep where
  sep [] = Nothing
  sep l  = Just . fmap (drop 1) . break (== chr) $ l

parseRule :: String -> Map.Map String [(String, Int)]
parseRule line = Map.singleton (kind ++ " " ++ color) (parseContent content)
    where kind:color:_:_:content = words line

parseContent :: [String] -> [(String, Int)]
parseContent ["no", "other", "bags."] = []
parseContent lst = map (\x -> ((x !! 1 ++ " " ++ x !! 2), (read (head x) :: Int))) $ map words $ separateBy ',' $ foldl1 ((++) . (++ " ")) lst

findAllPossibleContents :: Map.Map String [(String, Int)] -> Map.Map String [(String, Int)]
findAllPossibleContents bags = Map.map (findAllPossibleContents' bags) bags

findAllPossibleContents' :: Map.Map String [(String, Int)] -> [(String, Int)] -> [(String, Int)]
findAllPossibleContents' bags existing = (foldl (++) [] $ map (\x -> findAllPossibleContents' bags $ bags Map.! fst x) existing) ++ existing

countNeededBags :: Map.Map String [(String, Int)] -> String -> Int
countNeededBags _ [] = 0
countNeededBags bags key = sum $ map (\(x, y) -> y + y * countNeededBags bags x) (bags Map.! key)

main = do
    content <- getContents
    putStrLn $ show $ countNeededBags (foldl1 Map.union $ map parseRule $ lines content) "shiny gold"
    --putStrLn $ show $ Map.size $ Map.filter (== True) $ Map.map (\x -> any (== "shiny gold") $ map fst x) $ findAllPossibleContents $ foldl1 Map.union $ map parseRule $ lines content