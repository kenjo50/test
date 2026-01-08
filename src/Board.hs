module Board where  -- do NOT CHANGE export of module

-- IMPORTS HERE
-- Note: Imports allowed that DO NOT REQUIRE TO ANY CHANGES TO package.yaml, e.g.:
--       import Data.Chars
import Data.Char (isDigit, digitToInt)
import Text.Read()
import Data.List (intercalate)

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
buildPos [c, r]
  | c >= 'a' && c <= 'd' && r >= '0' && r <= '7' = Pos c (digitToInt r)
buildPos (c:rStr)
  | c >= 'a' && c <= 'd'
  , all isDigit rStr
  , let n = read rStr
  , n >= 0 && n <= 7
  = Pos c n
buildPos _ = Pos 'a' 0

-- ##############################################################################
-- ################## IMPLEMENT validateFEN :: String -> Bool ###################
-- ################## - 1 Functional Point                   ####################
-- ##############################################################################
-- AI-GENERIERT: Die folgende Implementierung wurde mit Unterstützung von
-- Claude Code (Anthropic) entwickelt.
-- ##############################################################################

validateFEN :: String -> Bool
validateFEN fen =
  let rows = splitOn '/' fen
  in length rows == 8 && all isValidRow rows
  where
    splitOn :: Char -> String -> [String]
    splitOn _ "" = [""]
    splitOn delim (x:xs)
      | x == delim = "" : splitOn delim xs
      | otherwise = case splitOn delim xs of
                      (y:ys) -> (x:y):ys
                      [] -> [[x]]

    isValidRow :: String -> Bool
    isValidRow "" = True  -- Empty rows are valid
    isValidRow row = all isValidChar row && rowLength row == 4

    isValidChar :: Char -> Bool
    isValidChar c = c `elem` "pdq1234"

    rowLength :: String -> Int
    rowLength = sum . map charValue

    charValue :: Char -> Int
    charValue c
      | c `elem` "pdq" = 1
      | isDigit c = digitToInt c
      | otherwise = 0

-- ##############################################################################
-- ################## IMPLEMENT buildBoard :: String -> Board ###################
-- ################## - 1 Functional Point                   ####################
-- ##############################################################################
-- AI-GENERIERT: Die folgende Implementierung wurde mit Unterstützung von
-- Claude Code (Anthropic) entwickelt.
-- ##############################################################################

buildBoard :: String -> Board
buildBoard fen = map parseRow (splitOn '/' fen)
  where
    splitOn :: Char -> String -> [String]
    splitOn _ "" = [""]
    splitOn delim (x:xs)
      | x == delim = "" : splitOn delim xs
      | otherwise = case splitOn delim xs of
                      (y:ys) -> (x:y):ys
                      [] -> [[x]]

    parseRow :: String -> [Cell]
    parseRow "" = [Empty, Empty, Empty, Empty]
    parseRow row = expandRow row []

    expandRow :: String -> [Cell] -> [Cell]
    expandRow "" acc = acc
    expandRow (c:cs) acc
      | c == 'p' = expandRow cs (acc ++ [Pawn])
      | c == 'd' = expandRow cs (acc ++ [Drone])
      | c == 'q' = expandRow cs (acc ++ [Queen])
      | isDigit c = expandRow cs (acc ++ replicate (digitToInt c) Empty)
      | otherwise = expandRow cs acc

-- ##############################################################################
-- ################## IMPLEMENT buildFEN :: Board -> String   ###################
-- ################## - 1 Functional Point                   ####################
-- ##############################################################################
-- AI-GENERIERT: Die folgende Implementierung wurde mit Unterstützung von
-- Claude Code (Anthropic) entwickelt.
-- ##############################################################################

buildFEN :: Board -> String
buildFEN board = intercalate "/" (map rowToFEN board)
  where
    rowToFEN :: [Cell] -> String
    rowToFEN row
      | all (== Empty) row = ""
      | otherwise = compressRow (map cellToChar row)

    cellToChar :: Cell -> Char
    cellToChar Pawn = 'p'
    cellToChar Drone = 'd'
    cellToChar Queen = 'q'
    cellToChar Empty = '0'  -- Temporary marker for empty cells

    compressRow :: String -> String
    compressRow = compress 0

    compress :: Int -> String -> String
    compress n "" = if n > 0 then [head (show n)] else ""
    compress n (c:cs)
      | c == '0' = compress (n + 1) cs
      | n > 0 = head (show n) : c : compress 0 cs
      | otherwise = c : compress 0 cs
