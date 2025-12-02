import Test.Hspec
import Board
import Logic

main :: IO ()
main = hspec $ do
    describe "Board.validateFEN" $ do
        it "validates correct starting position" $
            validateFEN "qqd1/qdp1/dpp1///1ppd/1pdq/1dqq" `shouldBe` True

        it "validates FEN with empty ranks" $
            validateFEN "q3/qdd1/1p2/3q/pp2/pppd//d1q1" `shouldBe` True

        it "validates FEN with empty rows" $
            validateFEN "q21/qdd1/1p11/12q/pp11/pppd//d1q1" `shouldBe` True

        it "rejects empty FEN string" $
            validateFEN "" `shouldBe` False

        it "rejects FEN with invalid characters" $
            validateFEN "qxd1/qdp1/dpp1///1ppd/1pdq/1dqq" `shouldBe` False

        it "rejects FEN with too many cells in a rank" $
            validateFEN "qqddd/qdp1/dpp1///1ppd/1pdq/1dqq" `shouldBe` False

        it "accepts FEN with all valid pieces" $
            validateFEN "ppp1/ddd1/qqq1" `shouldBe` True

    describe "Board.buildBoard" $ do
        it "builds board from starting position" $ do
            let board = buildBoard "qqd1/qdp1/dpp1///1ppd/1pdq/1dqq"
            length board `shouldBe` 8
            all (\row -> length row == 4) board `shouldBe` True

        it "builds board with empty ranks" $ do
            let board = buildBoard "q3/qdd1/1p2/3q/pp2/pppd//d1q1"
            board !! 0 !! 0 `shouldBe` Queen
            board !! 0 !! 1 `shouldBe` Empty

        it "handles empty rows correctly" $ do
            let board = buildBoard "////"
            all (\row -> all (== Empty) row) board `shouldBe` True

    describe "Board.buildFEN" $ do
        it "builds FEN from board" $ do
            let board = buildBoard "qqd1/qdp1/dpp1///1ppd/1pdq/1dqq"
                fen = buildFEN board
            validateFEN fen `shouldBe` True

        it "round-trips FEN string" $ do
            let originalFen = "qqd1/qdp1/dpp1///1ppd/1pdq/1dqq"
                board = buildBoard originalFen
                newFen = buildFEN board
                board2 = buildBoard newFen
            board `shouldBe` board2

        it "converts empty board to FEN" $ do
            let emptyBoard = replicate 8 (replicate 4 Empty)
                fen = buildFEN emptyBoard
            fen `shouldBe` "///////"

    describe "Logic.pawnMoves" $ do
        it "returns empty list for empty cell" $ do
            let board = buildBoard "///////"
                moves = pawnMoves board Bottom (Pos "a0") Nothing
            moves `shouldBe` []

        it "returns empty list for wrong player" $ do
            let board = buildBoard "//////p//"
                moves = pawnMoves board Bottom (Pos "a1") Nothing
            moves `shouldBe` []

        it "returns diagonal moves for pawn" $ do
            let board = buildBoard "//////p//"
                moves = pawnMoves board Top (Pos "a1") Nothing
            length moves `shouldSatisfy` (> 0)

        it "prevents takeback move" $ do
            let board = buildBoard "//////p//"
                lastMove = Just (Move (Pos "b2") (Pos "a1"))
                moves = pawnMoves board Top (Pos "a1") lastMove
            Move (Pos "a1") (Pos "b2") `notElem` moves `shouldBe` True

    describe "Logic.droneMoves" $ do
        it "returns empty list for empty cell" $ do
            let board = buildBoard "///////"
                moves = droneMoves board Bottom (Pos "a0") Nothing
            moves `shouldBe` []

        it "returns orthogonal moves for drone" $ do
            let board = buildBoard "/////d//"
                moves = droneMoves board Bottom (Pos "a2") Nothing
            length moves `shouldSatisfy` (> 0)

        it "limits movement by piece count" $ do
            let board = buildBoard "/////dppp//"
                moves = droneMoves board Bottom (Pos "a2") Nothing
            length moves `shouldSatisfy` (>= 1)

    describe "Logic.queenMoves" $ do
        it "returns empty list for empty cell" $ do
            let board = buildBoard "///////"
                moves = queenMoves board Bottom (Pos "a0") Nothing
            moves `shouldBe` []

        it "returns straight line moves for queen" $ do
            let board = buildBoard "////q///"
                moves = queenMoves board Top (Pos "a3") Nothing
            length moves `shouldSatisfy` (> 0)

        it "allows diagonal and orthogonal moves" $ do
            let board = buildBoard "////q///"
                moves = queenMoves board Top (Pos "a3") Nothing
            length moves `shouldSatisfy` (> 1)

    describe "Logic.makeMove" $ do
        it "moves piece to empty square" $ do
            let board = buildBoard "//////p//"
                move = Move (Pos "a1") (Pos "b2")
                (newBoard, points) = makeMove board move
                newFen = buildFEN newBoard
            points `shouldBe` 0

        it "captures piece across canal" $ do
            let board = buildBoard "///p/p///"
                move = Move (Pos "a4") (Pos "b3")
                (newBoard, points) = makeMove board move
            points `shouldSatisfy` (> 0)

        it "combines pawn and drone into queen" $ do
            let board = buildBoard "/////pd//"
                move = Move (Pos "a2") (Pos "b2")
                (newBoard, points) = makeMove board move
                newFen = buildFEN newBoard
            (newBoard !! 6) !! 1 `shouldBe` Queen

        it "combines two pawns into drone" $ do
            let board = buildBoard "/////pp//"
                move = Move (Pos "a2") (Pos "b2")
                (newBoard, points) = makeMove board move
            (newBoard !! 6) !! 1 `shouldBe` Drone

    describe "Logic.playerWon" $ do
        it "returns Nothing when game is not over" $ do
            let board = buildBoard "qqd1/qdp1/dpp1///1ppd/1pdq/1dqq"
                result = playerWon board Bottom 0 0
            result `shouldBe` Nothing

        it "returns winner when top zone is empty" $ do
            let board = buildBoard "///////dqp"
                result = playerWon board Bottom 5 10
            result `shouldBe` Just Bottom

        it "returns winner when bottom zone is empty" $ do
            let board = buildBoard "dqp////"
                result = playerWon board Top 10 5
            result `shouldBe` Just Top

        it "returns last player on tie" $ do
            let board = buildBoard "///////dqp"
                result = playerWon board Bottom 10 10
            result `shouldBe` Just Bottom

        it "returns higher score player" $ do
            let board = buildBoard "///////dqp"
                result = playerWon board Top 5 10
            result `shouldBe` Just Bottom
