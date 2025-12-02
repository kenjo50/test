module Board
    ( Cell(..)
    , Board
    , Player(..)
    , Pos(..)
    , Move(..)
    , validateFEN
    , buildBoard
    , buildFEN
    ) where

import Data.Char (isDigit)
import Data.List (intercalate)

-- | Represents a single cell on the board
data Cell = Empty | Pawn | Drone | Queen
    deriving (Eq, Show)

-- | Board is represented as a list of rows (from top to bottom)
-- Each row is a list of cells (from left to right)
-- (board!!0)!!0 is the top-left corner (a7)
-- (board!!0)!!3 is the top-right corner (d7)
type Board = [[Cell]]

-- | Players in the game
data Player = Top | Bottom
    deriving (Eq, Show, Read)

-- | Position on the board (algebraic notation: column + row, e.g., "a0", "d7")
data Pos = Pos String
    deriving (Eq, Show)

-- | Move from one position to another
data Move = Move Pos Pos
    deriving (Eq, Show)

-- Constants
numRows :: Int
numRows = 8

numCols :: Int
numCols = 4

-- | Validate a FEN string according to Martian Chess notation
validateFEN :: String -> Bool
validateFEN fen
    | null fen = False
    | otherwise =
        let ranks = splitOn '/' fen
        in length ranks <= numRows && all isValidRank ranks

-- Helper function to split string by delimiter
splitOn :: Char -> String -> [String]
splitOn delim str = case break (== delim) str of
    (chunk, []) -> [chunk]
    (chunk, _:rest) -> chunk : splitOn delim rest

-- | Check if a rank string is valid
isValidRank :: String -> Bool
isValidRank "" = True  -- Empty rank is valid
isValidRank rank =
    let (cells, empties) = foldl processChar ([], 0) rank
        totalCells = length cells + empties
    in totalCells == numCols && all isValidChar rank

-- Process a single character in a rank
processChar :: ([Cell], Int) -> Char -> ([Cell], Int)
processChar (cells, empties) ch
    | ch == 'p' = (cells ++ [Pawn], empties)
    | ch == 'd' = (cells ++ [Drone], empties)
    | ch == 'q' = (cells ++ [Queen], empties)
    | isDigit ch =
        let n = read [ch] :: Int
        in if n >= 1 && n <= numCols
           then (cells, empties + n)
           else (cells, empties + 100)  -- Invalid
    | otherwise = (cells, empties + 100)  -- Invalid

-- Check if character is valid in FEN
isValidChar :: Char -> Bool
isValidChar ch = ch `elem` "pqd" || (isDigit ch && ch >= '1' && ch <= '4')

-- | Build a board from a valid FEN string
buildBoard :: String -> Board
buildBoard fen =
    let ranks = splitOn '/' fen
        parsedRanks = map parseRank ranks
        -- Pad with empty rows if needed
        paddedRanks = parsedRanks ++ replicate (numRows - length parsedRanks) (replicate numCols Empty)
    in paddedRanks

-- | Parse a single rank from FEN notation
parseRank :: String -> [Cell]
parseRank "" = replicate numCols Empty
parseRank rank =
    let cells = concatMap charToCells rank
    in cells

-- Convert a FEN character to cells
charToCells :: Char -> [Cell]
charToCells 'p' = [Pawn]
charToCells 'd' = [Drone]
charToCells 'q' = [Queen]
charToCells ch
    | isDigit ch =
        let n = read [ch] :: Int
        in replicate n Empty
    | otherwise = []

-- | Build a FEN string from a board
buildFEN :: Board -> String
buildFEN board =
    let rankStrings = map rankToFEN board
        -- Replace "4" with "" for empty ranks
        cleanedRanks = map (\r -> if r == "4" then "" else r) rankStrings
    in intercalate "/" cleanedRanks

-- | Convert a single rank to FEN notation
rankToFEN :: [Cell] -> String
rankToFEN rank =
    let compressed = compressCells rank
    in if null compressed then "4" else compressed

-- Compress consecutive empty cells in a rank
compressCells :: [Cell] -> String
compressCells = compressHelper 0
    where
        compressHelper :: Int -> [Cell] -> String
        compressHelper 0 [] = ""
        compressHelper n [] = show n
        compressHelper empties (Empty:rest) = compressHelper (empties + 1) rest
        compressHelper 0 (cell:rest) = cellToChar cell : compressHelper 0 rest
        compressHelper n (cell:rest) = show n ++ (cellToChar cell : compressHelper 0 rest)

-- Convert a cell to its FEN character
cellToChar :: Cell -> Char
cellToChar Pawn = 'p'
cellToChar Drone = 'd'
cellToChar Queen = 'q'
cellToChar Empty = ' '  -- This shouldn't be called for Empty
