module Logic
    ( pawnMoves
    , droneMoves
    , queenMoves
    , makeMove
    , playerWon
    ) where

import Board

-- | Get all possible moves for a Pawn at the given position
pawnMoves :: Board -> Player -> Pos -> Maybe Move -> [Move]
pawnMoves board player pos lastMove =
    case posToRC pos of
        Nothing -> []
        Just (row, col) ->
            let cell = getCell board row col
            in if cell /= Pawn || not (isInPlayerZone player row)
               then []
               else filter (not . isTakeback lastMove) (pawnMovesFrom board row col)

-- | Get all possible moves for a Drone at the given position
droneMoves :: Board -> Player -> Pos -> Maybe Move -> [Move]
droneMoves board player pos lastMove =
    case posToRC pos of
        Nothing -> []
        Just (row, col) ->
            let cell = getCell board row col
            in if cell /= Drone || not (isInPlayerZone player row)
               then []
               else filter (not . isTakeback lastMove) (droneMovesFrom board row col)

-- | Get all possible moves for a Queen at the given position
queenMoves :: Board -> Player -> Pos -> Maybe Move -> [Move]
queenMoves board player pos lastMove =
    case posToRC pos of
        Nothing -> []
        Just (row, col) ->
            let cell = getCell board row col
            in if cell /= Queen || not (isInPlayerZone player row)
               then []
               else filter (not . isTakeback lastMove) (queenMovesFrom board row col)

-- | Execute a move and return the new board and points gained
makeMove :: Board -> Move -> (Board, Int)
makeMove board (Move from to) =
    case (posToRC from, posToRC to) of
        (Just (fromR, fromC), Just (toR, toC)) ->
            let piece = getCell board fromR fromC
                targetCell = getCell board toR toC
                crossingCanal = crossesCanal fromR toR
                -- Calculate points for capture
                points = if crossingCanal && targetCell /= Empty
                        then cellPoints targetCell
                        else 0
                -- Clear source cell
                board1 = setCell board fromR fromC Empty
                -- Place or combine piece at target
                finalBoard = handlePlacement board1 piece targetCell toR toC crossingCanal
            in (finalBoard, points)
        _ -> (board, 0)

-- | Check if a player has won
playerWon :: Board -> Player -> Int -> Int -> Maybe Player
playerWon board lastPlayer topScore bottomScore =
    let topEmpty = isZoneEmpty board Top
        bottomEmpty = isZoneEmpty board Bottom
    in if topEmpty || bottomEmpty
       then Just (determineWinner topScore bottomScore topEmpty bottomEmpty lastPlayer)
       else Nothing

-- Helper: Check if a zone is empty
isZoneEmpty :: Board -> Player -> Bool
isZoneEmpty board player =
    let rows = getZoneRows player
        cells = concatMap (\r -> board !! r) rows
    in all (== Empty) cells

-- Helper: Get rows for a player's zone
getZoneRows :: Player -> [Int]
getZoneRows Top = [0..3]     -- Rows 0-3 (ranks 7-4)
getZoneRows Bottom = [4..7]  -- Rows 4-7 (ranks 3-0)

-- Helper: Determine winner based on scores
determineWinner :: Int -> Int -> Bool -> Bool -> Player -> Player
determineWinner topScore bottomScore topEmpty bottomEmpty lastPlayer
    | topScore > bottomScore = Top
    | bottomScore > topScore = Bottom
    | otherwise = lastPlayer  -- Tie goes to player who ended the game

-- Helper: Handle piece placement or combination
handlePlacement :: Board -> Cell -> Cell -> Int -> Int -> Bool -> Board
handlePlacement board piece targetCell toR toC crossingCanal
    | crossingCanal = setCell board toR toC piece  -- Just place piece (capture already removed)
    | targetCell == Empty = setCell board toR toC piece  -- Move to empty square
    | otherwise = combinePieces board piece targetCell toR toC  -- Combine pieces in same zone

-- Helper: Combine two pieces according to promotion rules
-- Only combine if player doesn't have the resulting piece type in their zone
-- Note: board here is the intermediate state with source piece removed
combinePieces :: Board -> Cell -> Cell -> Int -> Int -> Board
combinePieces board piece1 piece2 toR toC =
    let player = if toR < 4 then Top else Bottom
        -- Check zone excluding the target square (since it will be replaced)
        hasQueens = playerHasPieceExcluding board player Queen toR toC
        hasDrones = playerHasPieceExcluding board player Drone toR toC
    in case (piece1, piece2) of
        (Pawn, Drone) -> if hasQueens
                         then setCell board toR toC piece1  -- Can't combine if already have queen
                         else setCell board toR toC Queen
        (Drone, Pawn) -> if hasQueens
                         then setCell board toR toC piece1
                         else setCell board toR toC Queen
        (Pawn, Pawn) -> if hasDrones
                        then setCell board toR toC piece1  -- Can't combine if already have drone
                        else setCell board toR toC Drone
        _ -> setCell board toR toC piece1  -- No combination, just place first piece

-- Helper: Check if player has a specific piece type in their zone (excluding a specific square)
playerHasPieceExcluding :: Board -> Player -> Cell -> Int -> Int -> Bool
playerHasPieceExcluding board player pieceType excludeR excludeC =
    let rows = getZoneRows player
        allCells = [(r, c, getCell board r c) | r <- rows, c <- [0..3]]
        relevantCells = [(r, c, cell) | (r, c, cell) <- allCells, r /= excludeR || c /= excludeC]
    in any (\(_, _, cell) -> cell == pieceType) relevantCells

-- Helper: Check if a move crosses the canal
crossesCanal :: Int -> Int -> Bool
crossesCanal fromRow toRow =
    (fromRow <= 3 && toRow >= 4) || (fromRow >= 4 && toRow <= 3)

-- Helper: Get points for capturing a piece
cellPoints :: Cell -> Int
cellPoints Pawn = 1
cellPoints Drone = 2
cellPoints Queen = 3
cellPoints Empty = 0

-- Helper: Check if a move is a takeback
isTakeback :: Maybe Move -> Move -> Bool
isTakeback Nothing _ = False
isTakeback (Just (Move lastFrom lastTo)) (Move from to) =
    lastTo == from && lastFrom == to

-- Helper: Get cell at position
getCell :: Board -> Int -> Int -> Cell
getCell board row col
    | row < 0 || row >= 8 || col < 0 || col >= 4 = Empty
    | otherwise = (board !! row) !! col

-- Helper: Set cell at position
setCell :: Board -> Int -> Int -> Cell -> Board
setCell board row col cell =
    let (before, current:after) = splitAt row board
        newRow = take col current ++ [cell] ++ drop (col + 1) current
    in before ++ [newRow] ++ after

-- Helper: Check if a row is in a player's zone
isInPlayerZone :: Player -> Int -> Bool
isInPlayerZone Top row = row >= 0 && row <= 3
isInPlayerZone Bottom row = row >= 4 && row <= 7

-- Helper: Convert position string to (row, col)
posToRC :: Pos -> Maybe (Int, Int)
posToRC (Pos str)
    | length str < 2 = Nothing
    | otherwise =
        let col = charToCol (head str)
            rowStr = tail str
        in case reads rowStr :: [(Int, String)] of
            [(rank, "")] ->
                if rank >= 0 && rank <= 7
                then Just (7 - rank, col)  -- Convert rank to row index
                else Nothing
            _ -> Nothing

-- Helper: Convert column character to index
charToCol :: Char -> Int
charToCol 'a' = 0
charToCol 'b' = 1
charToCol 'c' = 2
charToCol 'd' = 3
charToCol _ = -1

-- Helper: Convert (row, col) to position string
rcToPos :: Int -> Int -> Pos
rcToPos row col =
    let colChar = ['a', 'b', 'c', 'd'] !! col
        rank = 7 - row
    in Pos (colChar : show rank)

-- Get all pawn moves from a position
pawnMovesFrom :: Board -> Int -> Int -> [Move]
pawnMovesFrom board row col =
    let diagonals = [(row - 1, col - 1), (row - 1, col + 1),
                     (row + 1, col - 1), (row + 1, col + 1)]
        validMoves = filter (isValidMove board row col) diagonals
    in map (\(r, c) -> Move (rcToPos row col) (rcToPos r c)) validMoves

-- Check if a move is valid (works for all piece types)
isValidMove :: Board -> Int -> Int -> (Int, Int) -> Bool
isValidMove board fromR fromC (toR, toC)
    | toR < 0 || toR >= 8 || toC < 0 || toC >= 4 = False
    | otherwise =
        let targetCell = getCell board toR toC
            crossingCanal = crossesCanal fromR toR
        in if crossingCanal
           then targetCell /= Empty  -- Can only cross canal by capturing
           else True  -- Can move to empty or occupied square in same zone

-- Get all drone moves from a position
droneMovesFrom :: Board -> Int -> Int -> [Move]
droneMovesFrom board row col =
    let directions = [(0, 1), (0, -1), (1, 0), (-1, 0)]  -- Right, Left, Down, Up
        allMoves = concatMap (droneMovesInDirection board row col) directions
    in allMoves

-- Get drone moves in a specific direction
droneMovesInDirection :: Board -> Int -> Int -> (Int, Int) -> [Move]
droneMovesInDirection board row col (dr, dc) =
    let piecesInDirection = countPiecesInDirection board row col dr dc
        maxDistance = max 1 piecesInDirection
        positions = [(row + dr * i, col + dc * i) | i <- [1..maxDistance]]
        validPositions = collectValidMoves board row col positions
    in map (\(r, c) -> Move (rcToPos row col) (rcToPos r c)) validPositions

-- Count pieces in a direction
countPiecesInDirection :: Board -> Int -> Int -> Int -> Int -> Int
countPiecesInDirection board row col dr dc =
    let positions = [(row + dr * i, col + dc * i) | i <- [1..7]]
        inBounds (r, c) = r >= 0 && r < 8 && c >= 0 && c < 4
        validPositions = takeWhile inBounds positions
        cells = map (\(r, c) -> getCell board r c) validPositions
    in length (filter (/= Empty) cells)

-- Collect valid moves from a list of positions (stops at first occupied or invalid square)
-- Pieces can only move over empty squares (no jumping)
-- Can only cross canal by capturing (moving to occupied square across canal)
collectValidMoves :: Board -> Int -> Int -> [(Int, Int)] -> [(Int, Int)]
collectValidMoves _ _ _ [] = []
collectValidMoves board fromR fromC ((toR, toC):rest)
    | not (inBounds toR toC) = []
    | otherwise =
        let targetCell = getCell board toR toC
            crossingCanal = crossesCanal fromR toR
        in if crossingCanal
           then -- Crossing canal: can only capture (stop, only include if occupied)
                if targetCell /= Empty
                then [(toR, toC)]
                else []
           else -- Same zone movement
                if targetCell == Empty
                then (toR, toC) : collectValidMoves board fromR fromC rest
                else [(toR, toC)]  -- Stop at occupied square (can combine)

-- Helper: Check if position is in bounds
inBounds :: Int -> Int -> Bool
inBounds r c = r >= 0 && r < 8 && c >= 0 && c < 4

-- Get all queen moves from a position
queenMovesFrom :: Board -> Int -> Int -> [Move]
queenMovesFrom board row col =
    let directions = [(0, 1), (0, -1), (1, 0), (-1, 0),  -- Orthogonal
                      (1, 1), (1, -1), (-1, 1), (-1, -1)]  -- Diagonal
        allMoves = concatMap (queenMovesInDirection board row col) directions
    in allMoves

-- Get queen moves in a specific direction
queenMovesInDirection :: Board -> Int -> Int -> (Int, Int) -> [Move]
queenMovesInDirection board row col (dr, dc) =
    let positions = [(row + dr * i, col + dc * i) | i <- [1..7]]
        validPositions = collectValidMoves board row col positions
    in map (\(r, c) -> Move (rcToPos row col) (rcToPos r c)) validPositions
