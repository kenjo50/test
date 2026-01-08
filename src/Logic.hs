module Logic where  -- do NOT CHANGE export of module

-- IMPORTS HERE
-- Note: Imports allowed that DO NOT REQUIRE TO CHANGE package.yaml, e.g.:
--       import Data.Char

import Board
import Data.Char (ord)
import Data.Maybe (isJust, fromJust)

data Move = Move {start :: Pos, target :: Pos}

instance Show Move where
  show (Move (Pos startC startR) (Pos targetC targetR)) = [startC] ++ show startR ++ "-" ++ [targetC] ++ show targetR

instance Eq Move where
  (==) (Move (Pos sc1 sr1) (Pos tc1 tr1)) (Move (Pos sc2 sr2) (Pos tc2 tr2)) =
    sc1 == sc2 && sr1 == sr2 && tc1 == tc2 && tr1 == tr2

buildMove :: String -> Maybe Move
buildMove "" = Nothing
buildMove s = case break (=='-') s of
  (a, '-':b) -> Just (Move (buildPos a) (buildPos b))
  _ -> Nothing

-- ########################################################################################################
-- ################## HELPER FUNCTIONS                                                   ##################
-- ########################################################################################################
-- AI-GENERIERT: Die folgenden Hilfsfunktionen wurden mit Unterstützung von
-- Claude Code (Anthropic) entwickelt.
-- ########################################################################################################

-- Convert column character to index (a=0, b=1, c=2, d=3)
colToIndex :: Char -> Int
colToIndex c = ord c - ord 'a'

-- Convert index to column character
indexToCol :: Int -> Char
indexToCol i = toEnum (ord 'a' + i)

-- Get cell at position on board
getCell :: Board -> Pos -> Maybe Cell
getCell board (Pos c r)
  | r < 0 || r > 7 = Nothing
  | colIdx < 0 || colIdx > 3 = Nothing
  | otherwise = Just ((board !! (7 - r)) !! colIdx)
  where colIdx = colToIndex c

-- Check if position is valid
isValidPos :: Pos -> Bool
isValidPos (Pos c r) = r >= 0 && r <= 7 && c >= 'a' && c <= 'd'

-- Check which player owns the position based on zone
positionPlayer :: Pos -> Player
positionPlayer (Pos _ r)
  | r >= 4 = Top
  | otherwise = Bottom

-- Check if piece at position belongs to player
pieceBelongsToPlayer :: Board -> Player -> Pos -> Bool
pieceBelongsToPlayer board player pos =
  case getCell board pos of
    Nothing -> False
    Just Empty -> False
    Just _ -> positionPlayer pos == player

-- Check if move is a takeback (reverse of last move)
isTakeback :: Maybe Move -> Move -> Bool
isTakeback Nothing _ = False
isTakeback (Just (Move lastStart lastTarget)) (Move currentStart currentTarget) =
  lastTarget == currentStart && lastStart == currentTarget

-- Check if target crosses canal
crossesCanal :: Pos -> Pos -> Bool
crossesCanal (Pos _ r1) (Pos _ r2) = (r1 < 4 && r2 >= 4) || (r1 >= 4 && r2 < 4)

-- Get all valid diagonal moves for a position (distance = 1)
getDiagonalMoves :: Pos -> [Pos]
getDiagonalMoves (Pos c r) =
  filter isValidPos [
    Pos (toEnum (ord c - 1)) (r + 1),
    Pos (toEnum (ord c + 1)) (r + 1),
    Pos (toEnum (ord c - 1)) (r - 1),
    Pos (toEnum (ord c + 1)) (r - 1)
  ]

-- Get positions in a direction (dx, dy) up to maxDist
getPositionsInDirection :: Pos -> (Int, Int) -> Int -> [Pos]
getPositionsInDirection (Pos c r) (dx, dy) maxDist =
  take maxDist $ takeWhile isValidPos $ iterate step (Pos c r)
  where
    step (Pos c' r') = Pos (toEnum (ord c' + dx)) (r' + dy)

-- Count pieces in a direction
countPiecesInDirection :: Board -> Pos -> (Int, Int) -> Int
countPiecesInDirection board pos dir =
  let positions = tail $ getPositionsInDirection pos dir 8  -- tail to skip starting position
      cells = map (getCell board) positions
  in length $ filter isPiece cells
  where
    isPiece (Just Empty) = False
    isPiece (Just _) = True
    isPiece Nothing = False

-- Get orthogonal moves for drone (distance based on piece count)
getOrthogonalDroneMoves :: Board -> Pos -> [Pos]
getOrthogonalDroneMoves board pos =
  concatMap (getMovesInDirection pos) directions
  where
    directions = [(0, 1), (0, -1), (1, 0), (-1, 0)]  -- up, down, right, left
    getMovesInDirection from dir =
      let count = countPiecesInDirection board from dir
          maxDist = max 1 count
          positions = tail $ getPositionsInDirection from dir (maxDist + 1)
      in takeWhileInclusive (\p -> case getCell board p of Just Empty -> True; _ -> False) positions

    takeWhileInclusive :: (a -> Bool) -> [a] -> [a]
    takeWhileInclusive _ [] = []
    takeWhileInclusive p (x:xs)
      | p x = x : takeWhileInclusive p xs
      | otherwise = [x]

-- Get moves in all 8 directions (for queen)
getAllDirectionMoves :: Board -> Pos -> [Pos]
getAllDirectionMoves board pos =
  concatMap (getMovesInDirection pos) directions
  where
    directions = [(0, 1), (0, -1), (1, 0), (-1, 0), (1, 1), (1, -1), (-1, 1), (-1, -1)]
    getMovesInDirection from dir =
      let positions = tail $ getPositionsInDirection from dir 8
      in takeWhileInclusive (\p -> case getCell board p of Just Empty -> True; _ -> False) positions

    takeWhileInclusive :: (a -> Bool) -> [a] -> [a]
    takeWhileInclusive _ [] = []
    takeWhileInclusive p (x:xs)
      | p x = x : takeWhileInclusive p xs
      | otherwise = [x]

-- ########################################################################################################
-- ################## pawnMoves :: Board -> Player -> Pos -> Maybe Move -> [Move]        ##################
-- ################## - 5 Functional Points                                              ##################
-- ########################################################################################################
-- AI-GENERIERT: Die folgende Implementierung wurde mit Unterstützung von
-- Claude Code (Anthropic) entwickelt.
-- ########################################################################################################

pawnMoves :: Board -> Player -> Pos -> Maybe Move -> [Move]
pawnMoves board player pos lastMove =
  case getCell board pos of
    Just Pawn | pieceBelongsToPlayer board player pos ->
      let targets = getDiagonalMoves pos
          validTargets = filter (isValidPawnTarget pos) targets
          moves = map (Move pos) validTargets
      in filter (not . isTakeback lastMove) moves
    _ -> []
  where
    isValidPawnTarget :: Pos -> Pos -> Bool
    isValidPawnTarget from to =
      case getCell board to of
        Just Empty -> True  -- Can move to empty in same zone or across canal
        Just _ -> crossesCanal from to  -- Can capture only if crossing canal
        Nothing -> False

-- #######################################################################################################
-- ################## droneMoves :: Board -> Player -> Pos -> Maybe Move -> [Move]      ##################
-- ################## - 5 Functional Points                                             ##################
-- #######################################################################################################
-- AI-GENERIERT: Die folgende Implementierung wurde mit Unterstützung von
-- Claude Code (Anthropic) entwickelt.
-- #######################################################################################################

droneMoves :: Board -> Player -> Pos -> Maybe Move -> [Move]
droneMoves board player pos lastMove =
  case getCell board pos of
    Just Drone | pieceBelongsToPlayer board player pos ->
      let targets = getOrthogonalDroneMoves board pos
          moves = map (Move pos) targets
          validMoves = filter (isValidDroneMove pos) moves
      in filter (not . isTakeback lastMove) validMoves
    _ -> []
  where
    isValidDroneMove :: Pos -> Move -> Bool
    isValidDroneMove from (Move _ to) =
      case getCell board to of
        Just Empty -> True  -- Can move to empty in same zone or across canal
        Just _ -> crossesCanal from to  -- Can only capture if crossing canal
        Nothing -> False

-- #######################################################################################################
-- ################## queenMoves :: Board -> Player -> Pos -> Maybe Move -> [Move]      ##################
-- ################## - 3 Functional Points                                             ##################
-- #######################################################################################################
-- AI-GENERIERT: Die folgende Implementierung wurde mit Unterstützung von
-- Claude Code (Anthropic) entwickelt.
-- #######################################################################################################

queenMoves :: Board -> Player -> Pos -> Maybe Move -> [Move]
queenMoves board player pos lastMove =
  case getCell board pos of
    Just Queen | pieceBelongsToPlayer board player pos ->
      let targets = getAllDirectionMoves board pos
          moves = map (Move pos) targets
          validMoves = filter (isValidQueenMove pos) moves
      in filter (not . isTakeback lastMove) validMoves
    _ -> []
  where
    isValidQueenMove :: Pos -> Move -> Bool
    isValidQueenMove from (Move _ to) =
      case getCell board to of
        Just Empty -> True  -- Can always move to empty
        Just _ -> crossesCanal from to  -- Can only capture if crossing canal
        Nothing -> False

-- #######################################################################################################
-- ################## makeMove :: Board -> Move -> (Board -> Int)                       ##################
-- ################## - 3 Functional Points                                             ##################
-- #######################################################################################################
-- AI-GENERIERT: Die folgende Implementierung wurde mit Unterstützung von
-- Claude Code (Anthropic) entwickelt.
-- #######################################################################################################

makeMove :: Board -> Move -> (Board, Int)
makeMove board (Move from to) =
  case (getCell board from, getCell board to) of
    (Just piece, Just targetCell) ->
      let points = cellValue targetCell
          -- Remove piece from start position
          board1 = setCell board from Empty
          -- Place piece at target position
          board2 = setCell board1 to piece
      in (board2, points)
    _ -> (board, 0)
  where
    cellValue :: Cell -> Int
    cellValue Pawn = 1
    cellValue Drone = 2
    cellValue Queen = 3
    cellValue Empty = 0

    setCell :: Board -> Pos -> Cell -> Board
    setCell b (Pos c r) cell =
      let rowIdx = 7 - r
          colIdx = colToIndex c
          row = b !! rowIdx
          newRow = take colIdx row ++ [cell] ++ drop (colIdx + 1) row
      in take rowIdx b ++ [newRow] ++ drop (rowIdx + 1) b

-- #######################################################################################################
-- ################## playerWon :: Board -> Player -> Int -> Int -> Maybe Player        ##################
-- ################## - 3 Functional Points                                             ##################
-- #######################################################################################################
-- AI-GENERIERT: Die folgende Implementierung wurde mit Unterstützung von
-- Claude Code (Anthropic) entwickelt.
-- #######################################################################################################

playerWon :: Board -> Player -> Int -> Int -> Maybe Player
playerWon board lastPlayer topScore bottomScore
  | isZoneEmpty board Top = Just (winner bottomScore topScore Bottom)
  | isZoneEmpty board Bottom = Just (winner topScore bottomScore Top)
  | otherwise = Nothing
  where
    isZoneEmpty :: Board -> Player -> Bool
    isZoneEmpty b player =
      let positions = case player of
            Top -> [Pos c r | c <- ['a'..'d'], r <- [4..7]]
            Bottom -> [Pos c r | c <- ['a'..'d'], r <- [0..3]]
          cells = map (getCell b) positions
      in all (\c -> c == Just Empty || c == Nothing) cells

    winner :: Int -> Int -> Player -> Player
    winner score1 score2 player1
      | score1 > score2 = player1
      | score1 < score2 = if player1 == Top then Bottom else Top
      | otherwise = lastPlayer  -- Tie: last player wins

