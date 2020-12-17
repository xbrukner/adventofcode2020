import Data.List

-- https://stackoverflow.com/a/4978733
separateBy :: Eq a => a -> [a] -> [[a]]
separateBy chr = unfoldr sep where
  sep [] = Nothing
  sep l  = Just . fmap (drop 1) . break (== chr) $ l



main = do
    content <- getContents
    --putStrLn $ show $ sum $ map length $ map (foldl1 union) $ separateBy "" $ lines content 
    putStrLn $ show $ sum $ map length $ map (foldl1 intersect) $ separateBy "" $ lines content 