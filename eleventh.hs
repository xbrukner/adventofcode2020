import Data.List
data Seat = Empty | Occupied | Floor deriving (Enum, Eq, Show)

parseSeat :: Char -> Seat
parseSeat 'L' = Empty
parseSeat '#' = Occupied
parseSeat '.' = Floor
parseSeat x = error ("Invalid seat " ++ x:[])

findAdjacent :: [[Seat]] -> Int -> Int -> [Seat]
findAdjacent seats row column
    | row == 0 = findAdjacent' [currentRow, nextRow] column
    | row == length seats - 1 = findAdjacent' [previousRow, currentRow] column
    | otherwise = findAdjacent' [previousRow, currentRow, nextRow] column
    where
        previousRow = seats !! (row - 1)
        currentRow = seats !! row
        nextRow = seats !! (row + 1)
findAdjacent' :: [[Seat]] -> Int -> [Seat]
findAdjacent' seats column
    | column == 0 = concat [currentSeats, nextSeats]
    | column == length (head seats) - 1 = concat [previousSeats, currentSeats]
    | otherwise = concat [previousSeats, currentSeats, nextSeats]
    where
        previousSeats = map (!! (column - 1)) seats
        currentSeats = map (!! column) seats
        nextSeats = map (!! (column + 1)) seats

lookAdjacent :: [[Seat]] -> Int -> Int -> [Seat]
lookAdjacent seats row column = map (\movement -> findFirst seats (move (row, column) movement) movement) movements
    where movements = [(rowMovement, columnMovement) | rowMovement <- [-1..1], columnMovement <- [-1..1], rowMovement /= 0 || columnMovement /= 0]

findFirst :: [[Seat]] -> (Int, Int) -> (Int, Int) -> Seat
findFirst seats position movement
    | inBounds (bounds seats) position == False = Floor --could be Nothing, but Floor is effectivelly Nothing
    | getSeat seats position == Floor = findFirst seats (move position movement) movement
    | otherwise = getSeat seats position

move :: (Int, Int) -> (Int, Int) -> (Int, Int)
move (row, column) (moveRow, moveColumn) = (row + moveRow, column + moveColumn)

getSeat :: [[Seat]] -> (Int, Int) -> Seat
getSeat seats (row, column) = seats !! row !! column

bounds :: [[Seat]] -> (Int, Int)
bounds seats = (length seats, length $ head seats)

inBounds :: (Int, Int) -> (Int, Int) -> Bool
inBounds _ (-1, _) = False
inBounds _ (_, -1) = False
inBounds (rows, _) (row, _)
    | row == rows = False
inBounds (_, columns) (_, column)
    | column == columns = False
inBounds _ _ = True

countOccupiedAround :: [[Seat]] -> Int -> Int -> Int
--countOccupiedAround seats row column = length $ filter (==Occupied) $ (findAdjacent seats row column) \\ [(seats !! row !! column)]
countOccupiedAround seats row column = length $ filter (==Occupied) $ lookAdjacent seats row column

getNewState :: [[Seat]] -> [[Seat]]
getNewState seats = map (\row -> map (updateSeat seats row) [0..length (head seats) - 1]) [0..(length seats - 1)]

updateSeat :: [[Seat]] -> Int -> Int -> Seat
updateSeat seats row column
    | seats !! row !! column == Empty && occupied == 0 = Occupied
    | seats !! row !! column == Occupied && occupied >= 5 = Empty --4 for part 1
    | otherwise = seats !! row !! column
    where occupied = countOccupiedAround seats row column

findStableSeating :: [[Seat]] -> [[Seat]]
findStableSeating seats
    | seats == newSeats = seats
    | otherwise = findStableSeating newSeats
    where newSeats = getNewState seats

main = do
    content <- getContents
    putStrLn $ show $ length $ filter (== Occupied) $ concat $ findStableSeating $ map (map parseSeat) $ lines content