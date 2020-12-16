{-# LANGUAGE NamedFieldPuns #-}
import Data.List (unfoldr)

data PasswordPolicy = PasswordPolicy {
    from :: Integer,
    to :: Integer,
    letter :: Char,
    password :: String
} deriving (Show)

isValidCount :: PasswordPolicy -> Bool
isValidCount (PasswordPolicy {from, to, letter, password}) = from <= count && to >= count
    where count = toInteger $ length $ filter (== letter) password

isValidPosition :: PasswordPolicy -> Bool
isValidPosition (PasswordPolicy {from, to, letter, password}) = (extractPosition from) /= (extractPosition to)
    where extractPosition = \pos -> (password !! (fromIntegral pos - 1) == letter)

parsePasswordPolicy :: String -> PasswordPolicy
parsePasswordPolicy input = let
    [ranges, letter, password] = words input
    [from, to] = map read $ separateBy '-' ranges
    in PasswordPolicy{from = from, to = to, letter = head letter, password = password}

-- https://stackoverflow.com/a/4978733

separateBy :: Eq a => a -> [a] -> [[a]]
separateBy chr = unfoldr sep where
  sep [] = Nothing
  sep l  = Just . fmap (drop 1) . break (== chr) $ l

main = do
    content <- getContents
    putStrLn $ show $ length $ filter (== True) $ map (isValidPosition . parsePasswordPolicy) $ lines content