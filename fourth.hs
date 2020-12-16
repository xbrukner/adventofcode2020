import Data.List
import qualified Data.Map.Lazy as Map

required = Map.fromList [
    ("byr", (\x -> let value = read x in value >= 1920 && value <= 2002)), 
    ("iyr", (\x -> let value = read x in value >= 2010 && value <= 2020)),
    ("eyr", (\x -> let value = read x in value >= 2020 && value <= 2030)),
    ("hgt", checkHeightReversed . splitAt 2 . reverse),
    ("hcl", (\el -> let (x:xs) = el in x == '#' && length xs == 6 && all (`elem` ['0'..'9'] ++ ['a'..'f']) xs)),
    ("ecl", (\x -> x `elem` ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"])),
    ("pid", (\x -> length x == 9 && all (`elem` ['0'..'9']) x)),
    ("cid", (const True))]

checkHeightReversed :: (String, String) -> Bool
checkHeightReversed ("ni", size) = let value = read (reverse size) in value >= 59 && value <= 76
checkHeightReversed ("mc", size) = let value = read (reverse size) in value >= 150 && value <= 193
checkHeightReversed _ = False

extractKeys :: String -> Map.Map String String
extractKeys = foldl (\m [k, v] -> Map.insert k v m) Map.empty . map (separateBy ':') . words

allKeys :: Map.Map String String -> Bool
allKeys x = diff == [] || diff == ["cid"]
    where diff = (Map.keys required) \\ (Map.keys x)

validateKeys :: Map.Map String String -> Map.Map String (String -> Bool) -> Map.Map String Bool
validateKeys input rules = Map.foldlWithKey (\res key value -> Map.insert key ((rules Map.! key) value) res) Map.empty input


extractBatches :: String -> [String]
extractBatches = map (foldl1 ((++) . (++ " "))) . separateBy "" . lines

-- https://stackoverflow.com/a/4978733
separateBy :: Eq a => a -> [a] -> [[a]]
separateBy chr = unfoldr sep where
  sep [] = Nothing
  sep l  = Just . fmap (drop 1) . break (== chr) $ l

main = do
    content <- getContents
    --putStrLn $ show $ map length $ map extractKeys $ extractBatches content
    putStrLn $ show $ length $ filter (== True) $ map (\x -> allKeys x && all (== True) (validateKeys x required)) $ map extractKeys $ extractBatches content