module Logic where  -- do NOT CHANGE export of module

-- IMPORTS HERE
-- Note: Imports allowed that DO NOT REQUIRE TO CHANGE package.yaml, e.g.:
--       import Data.Char

import Board
import Data.Char()
import Data.Maybe()

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
  _ -> error "Invalid move format"

-- ########################################################################################################
-- ################## pawnMoves :: Board -> Player -> Pos -> Maybe Move -> [Move]        ##################
-- ################## - 5 Functional Points                                              ##################
-- ########################################################################################################

pawnMoves :: Board -> Player -> Pos -> Maybe Move -> [Move]
pawnMoves _ _ _ _ = []

-- #######################################################################################################
-- ################## droneMoves :: Board -> Player -> Pos -> Maybe Move -> [Move]      ##################
-- ################## - 5 Functional Points                                             ##################
-- #######################################################################################################

droneMoves :: Board -> Player -> Pos -> Maybe Move -> [Move]
droneMoves _ _ _ _ = []

-- #######################################################################################################
-- ################## queenMoves :: Board -> Player -> Pos -> Maybe Move -> [Move]      ##################
-- ################## - 3 Functional Points                                             ##################
-- #######################################################################################################

queenMoves :: Board -> Player -> Pos -> Maybe Move -> [Move]
queenMoves _ _ _ _ = []

-- #######################################################################################################
-- ################## makeMove :: Board -> Move -> (Board -> Int)                       ##################
-- ################## - 3 Functional Points                                             ##################
-- #######################################################################################################

makeMove :: Board -> Move -> (Board, Int)
makeMove board _ = (board, 0)

-- #######################################################################################################
-- ################## playerWon :: Board -> Player -> Int -> Int -> Maybe Player        ##################
-- ################## - 3 Functional Points                                             ##################
-- #######################################################################################################

playerWon :: Board -> Player -> Int -> Int -> Maybe Player
playerWon _ _ _ _ = Nothing
