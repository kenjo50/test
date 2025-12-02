module Main (main) where

import System.Environment (getArgs)
import Board
import Logic

main :: IO ()
main = do
    args <- getArgs
    case args of
        ("pawnMoves":fenStr:playerStr:posStr:lastMoveStr:_) -> do
            let board = buildBoard fenStr
                player = read playerStr :: Player
                pos = Pos posStr
                lastMove = parseLastMove lastMoveStr
                moves = pawnMoves board player pos lastMove
            putStrLn (show (map showMove moves))

        ("droneMoves":fenStr:playerStr:posStr:lastMoveStr:_) -> do
            let board = buildBoard fenStr
                player = read playerStr :: Player
                pos = Pos posStr
                lastMove = parseLastMove lastMoveStr
                moves = droneMoves board player pos lastMove
            putStrLn (show (map showMove moves))

        ("queenMoves":fenStr:playerStr:posStr:lastMoveStr:_) -> do
            let board = buildBoard fenStr
                player = read playerStr :: Player
                pos = Pos posStr
                lastMove = parseLastMove lastMoveStr
                moves = queenMoves board player pos lastMove
            putStrLn (show (map showMove moves))

        ("makeMove":fenStr:moveStr:_) -> do
            let board = buildBoard fenStr
                move = parseMove moveStr
                (newBoard, points) = makeMove board move
                newFen = buildFEN newBoard
            putStrLn ("(" ++ newFen ++ "," ++ show points ++ ")")

        ("playerWon":fenStr:lastPlayerStr:topScoreStr:bottomScoreStr:_) -> do
            let board = buildBoard fenStr
                lastPlayer = read lastPlayerStr :: Player
                topScore = read topScoreStr :: Int
                bottomScore = read bottomScoreStr :: Int
                result = playerWon board lastPlayer topScore bottomScore
            case result of
                Nothing -> putStrLn "Nothing"
                Just winner -> putStrLn (show winner)

        _ -> putStrLn "Invalid command"

-- Helper: Parse last move from string
parseLastMove :: String -> Maybe Move
parseLastMove "" = Nothing
parseLastMove str
    | str == "None" || str == "nothing" || str == "" = Nothing
    | '-' `elem` str =
        let (from, rest) = break (== '-') str
            to = if null rest then "" else tail rest
        in if null from || null to
           then Nothing
           else Just (Move (Pos from) (Pos to))
    | otherwise = Nothing

-- Helper: Parse move from string
parseMove :: String -> Move
parseMove str =
    let (from, rest) = break (== '-') str
        to = if null rest then "" else tail rest
    in Move (Pos from) (Pos to)

-- Helper: Show move in string format
showMove :: Move -> String
showMove (Move (Pos from) (Pos to)) = from ++ "-" ++ to
