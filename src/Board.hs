module Board where  -- do NOT CHANGE export of module

-- IMPORTS HERE
-- Note: Imports allowed that DO NOT REQUIRE TO ANY CHANGES TO package.yaml, e.g.:
--       import Data.Chars
import Data.Char()
import Text.Read()

-- #############################################################################
-- ############# GIVEN IMPLEMENTATION                           ################
-- ############# Given data types may NOT be changed            ################
-- #############################################################################

data Player = Top | Bottom deriving (Show, Read)
data Cell = Empty | Queen | Drone | Pawn deriving Show
data Pos = Pos { col :: Char, row :: Int } deriving Show
type Board = [[Cell]]

instance Eq Pos where
  (==) (Pos c1 r1) (Pos c2 r2) = (c1 == c2) && (r1 == r2)

instance Eq Player where
  (==) Top Top = True
  (==) Bottom Bottom = True
  (==) _ _ = False

instance Eq Cell where
  (==) Empty Empty = True
  (==) Pawn Pawn = True
  (==) Drone Drone = True
  (==) Queen Queen = True
  (==) _ _ = False
  
startingFEN :: String
startingFEN = "qqd1/qdp1/dpp1///1ppd/1pdq/1dqq"

startingBoard :: [[Cell]]
startingBoard = [
  [Queen, Queen, Drone, Empty],
  [Queen, Drone, Pawn,  Empty],
  [Drone, Pawn,  Pawn,  Empty],
  [Empty, Empty, Empty, Empty],
  [Empty, Empty, Empty, Empty],
  [Empty, Pawn,  Pawn,  Drone],
  [Empty, Pawn,  Drone, Queen],
  [Empty, Drone, Queen, Queen]
  ]

buildPos :: String -> Pos 
buildPos (c:rStr) = Pos c (read rStr) 
buildPos _ = error "Invalid position format"

-- ##############################################################################
-- ################## IMPLEMENT validateFEN :: String -> Bool ###################
-- ################## - 1 Functional Point                   ####################
-- ##############################################################################

validateFEN :: String -> Bool
validateFEN _ = True

-- ##############################################################################
-- ################## IMPLEMENT buildBoard :: String -> Board ###################
-- ################## - 1 Functional Point                   ####################
-- ##############################################################################

buildBoard :: String -> Board
buildBoard _ = startingBoard

-- ##############################################################################
-- ################## IMPLEMENT buildFEN :: Board -> String   ###################
-- ################## - 1 Functional Point                   ####################
-- ##############################################################################

buildFEN :: Board -> String
buildFEN board = startingFEN
