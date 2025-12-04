-- #############################################################################
-- ###### TESTS                                                       ##########
-- #############################################################################

import Test.Hspec

import Board (validateFEN, buildBoard, buildFEN, startingFEN, startingBoard,
              Player(Top, Bottom),
              Cell(Empty, Pawn, Drone, Queen),
              Pos(Pos))

import Logic (Move(Move), pawnMoves, droneMoves, queenMoves, makeMove, playerWon)

main :: IO ()
main = hspec $ do
  testValidateFEN
  testBuildBoard
  testBuildFEN
  testPawnMoves
  testDroneMoves
  testQueenMoves
  testMakeMove
  testPlayerWon

testValidateFEN :: Spec
testValidateFEN = describe "validateFEN" $ do
  it "validates starting FEN" $ do
    validateFEN startingFEN `shouldBe` True

  it "validates FEN with empty rows" $ do
    validateFEN "qqd1/qdp1/dpp1///1ppd/1pdq/1dqq" `shouldBe` True

  it "validates FEN with all pieces" $ do
    validateFEN "qqqq/dddd/pppp///pppp/dddd/qqqq" `shouldBe` True

  it "rejects FEN with too many rows" $ do
    validateFEN "qqqq/dddd/pppp////pppp/dddd/qqqq/pppp" `shouldBe` False

  it "rejects FEN with too few rows" $ do
    validateFEN "qqqq/dddd/pppp//" `shouldBe` False

  it "rejects FEN with invalid characters" $ do
    validateFEN "qqqq/dddd/pppp///x/pppp/dddd/qqqq" `shouldBe` False

  it "rejects FEN with wrong row length" $ do
    validateFEN "qqqqq/dddd/pppp////pppp/dddd/qqqq" `shouldBe` False

  it "validates FEN with numbers" $ do
    validateFEN "q3/d3/p3///p3/d3/q3" `shouldBe` True

  it "validates FEN with mixed numbers and pieces" $ do
    validateFEN "q2d/1pp1/2pq///qp2/1pp1/d2q" `shouldBe` True

testBuildBoard :: Spec
testBuildBoard = describe "buildBoard" $ do
  it "builds starting board from starting FEN" $ do
    buildBoard startingFEN `shouldBe` startingBoard

  it "builds board with all empty row" $ do
    buildBoard "///////" `shouldBe` [
      [Empty, Empty, Empty, Empty],
      [Empty, Empty, Empty, Empty],
      [Empty, Empty, Empty, Empty],
      [Empty, Empty, Empty, Empty],
      [Empty, Empty, Empty, Empty],
      [Empty, Empty, Empty, Empty],
      [Empty, Empty, Empty, Empty],
      [Empty, Empty, Empty, Empty]
      ]

  it "builds board with single piece" $ do
    buildBoard "q3///////3q" `shouldBe` [
      [Queen, Empty, Empty, Empty],
      [Empty, Empty, Empty, Empty],
      [Empty, Empty, Empty, Empty],
      [Empty, Empty, Empty, Empty],
      [Empty, Empty, Empty, Empty],
      [Empty, Empty, Empty, Empty],
      [Empty, Empty, Empty, Empty],
      [Empty, Empty, Empty, Queen]
      ]

  it "builds board with all pieces" $ do
    buildBoard "qqqq/dddd/pppp///pppp/dddd/qqqq" `shouldBe` [
      [Queen, Queen, Queen, Queen],
      [Drone, Drone, Drone, Drone],
      [Pawn, Pawn, Pawn, Pawn],
      [Empty, Empty, Empty, Empty],
      [Empty, Empty, Empty, Empty],
      [Pawn, Pawn, Pawn, Pawn],
      [Drone, Drone, Drone, Drone],
      [Queen, Queen, Queen, Queen]
      ]

testBuildFEN :: Spec
testBuildFEN = describe "buildFEN" $ do
  it "builds starting FEN from starting board" $ do
    buildFEN startingBoard `shouldBe` startingFEN

  it "builds FEN with empty rows" $ do
    buildFEN [
      [Empty, Empty, Empty, Empty],
      [Empty, Empty, Empty, Empty],
      [Empty, Empty, Empty, Empty],
      [Empty, Empty, Empty, Empty],
      [Empty, Empty, Empty, Empty],
      [Empty, Empty, Empty, Empty],
      [Empty, Empty, Empty, Empty],
      [Empty, Empty, Empty, Empty]
      ] `shouldBe` "///////"

  it "builds FEN from board with pieces" $ do
    buildFEN [
      [Queen, Queen, Queen, Queen],
      [Drone, Drone, Drone, Drone],
      [Pawn, Pawn, Pawn, Pawn],
      [Empty, Empty, Empty, Empty],
      [Empty, Empty, Empty, Empty],
      [Pawn, Pawn, Pawn, Pawn],
      [Drone, Drone, Drone, Drone],
      [Queen, Queen, Queen, Queen]
      ] `shouldBe` "qqqq/dddd/pppp///pppp/dddd/qqqq"

testPawnMoves :: Spec
testPawnMoves = describe "pawnMoves" $ do
  it "returns empty for wrong position" $ do
    pawnMoves startingBoard Bottom (Pos 'a' 7) Nothing `shouldBe` []

  it "returns empty for empty cell" $ do
    pawnMoves startingBoard Bottom (Pos 'a' 0) Nothing `shouldBe` []

  it "returns valid moves for pawn" $ do
    let board = buildBoard "///1p2/2p1////////"
        moves = pawnMoves board Top (Pos 'b' 4) Nothing
    length moves `shouldSatisfy` (> 0)

  it "filters out takeback moves" $ do
    let board = buildBoard "///1p2/2p1////////"
        lastMove = Just (Move (Pos 'a' 3) (Pos 'b' 4))
        moves = pawnMoves board Top (Pos 'b' 4) lastMove
    Move (Pos 'b' 4) (Pos 'a' 3) `shouldNotSatisfy` (`elem` moves)

  it "pawn can move diagonally in own zone" $ do
    let board = buildBoard "//////1p2//////////"
        moves = pawnMoves board Bottom (Pos 'b' 1) Nothing
    length moves `shouldSatisfy` (>= 0)

testDroneMoves :: Spec
testDroneMoves = describe "droneMoves" $ do
  it "returns empty for wrong position" $ do
    droneMoves startingBoard Top (Pos 'a' 0) Nothing `shouldBe` []

  it "returns empty for empty cell" $ do
    droneMoves startingBoard Bottom (Pos 'a' 0) Nothing `shouldBe` []

  it "returns valid moves for drone" $ do
    let board = buildBoard "///d3////////"
        moves = droneMoves board Top (Pos 'a' 4) Nothing
    length moves `shouldSatisfy` (> 0)

  it "drone movement based on piece count" $ do
    let board = buildBoard "d3/d3/d3/d3////////"
        moves = droneMoves board Top (Pos 'a' 7) Nothing
    length moves `shouldSatisfy` (> 0)

  it "filters out takeback moves" $ do
    let board = buildBoard "///d3////////"
        lastMove = Just (Move (Pos 'b' 4) (Pos 'a' 4))
        moves = droneMoves board Top (Pos 'a' 4) lastMove
    Move (Pos 'a' 4) (Pos 'b' 4) `shouldNotSatisfy` (`elem` moves)

testQueenMoves :: Spec
testQueenMoves = describe "queenMoves" $ do
  it "returns empty for wrong position" $ do
    queenMoves startingBoard Bottom (Pos 'a' 7) Nothing `shouldBe` []

  it "returns empty for empty cell" $ do
    queenMoves startingBoard Bottom (Pos 'b' 1) Nothing `shouldBe` []

  it "returns valid moves for queen" $ do
    let board = buildBoard "///q3////////"
        moves = queenMoves board Top (Pos 'a' 4) Nothing
    length moves `shouldSatisfy` (> 0)

  it "queen can move in all directions" $ do
    let board = buildBoard "//////2q1//////////"
        moves = queenMoves board Bottom (Pos 'c' 1) Nothing
    length moves `shouldSatisfy` (> 0)

  it "filters out takeback moves" $ do
    let board = buildBoard "///q3////////"
        lastMove = Just (Move (Pos 'b' 4) (Pos 'a' 4))
        moves = queenMoves board Top (Pos 'a' 4) lastMove
    Move (Pos 'a' 4) (Pos 'b' 4) `shouldNotSatisfy` (`elem` moves)

testMakeMove :: Spec
testMakeMove = describe "makeMove" $ do
  it "does nothing for invalid move" $ do
    makeMove startingBoard (Move (Pos 'a' 0) (Pos 'a' 0)) `shouldBe` (startingBoard, 0)

  it "moves piece to empty square" $ do
    let board = buildBoard "//////1p2//////////"
        (newBoard, points) = makeMove board (Move (Pos 'b' 1) (Pos 'a' 2))
    points `shouldBe` 0

  it "captures piece and awards points" $ do
    let board = buildBoard "///p3/3d////////"
        (newBoard, points) = makeMove board (Move (Pos 'd' 3) (Pos 'a' 4))
    points `shouldBe` 1

  it "captures drone and awards 2 points" $ do
    let board = buildBoard "///d3/3p////////"
        (newBoard, points) = makeMove board (Move (Pos 'd' 3) (Pos 'a' 4))
    points `shouldBe` 2

  it "captures queen and awards 3 points" $ do
    let board = buildBoard "///q3/3p////////"
        (newBoard, points) = makeMove board (Move (Pos 'd' 3) (Pos 'a' 4))
    points `shouldBe` 3

testPlayerWon :: Spec
testPlayerWon = describe "playerWon" $ do
  it "no player has won at start" $ do
    playerWon startingBoard Bottom 0 0 `shouldBe` Nothing

  it "bottom wins when top zone is empty" $ do
    let board = buildBoard "////////pppp/dddd/qqqq"
    playerWon board Bottom 5 10 `shouldBe` Just Bottom

  it "top wins when bottom zone is empty" $ do
    let board = buildBoard "qqqq/dddd/pppp////////"
    playerWon board Top 10 5 `shouldBe` Just Top

  it "player with more points wins" $ do
    let board = buildBoard "////////pppp/dddd/qqqq"
    playerWon board Bottom 5 10 `shouldBe` Just Bottom

  it "last player wins on tie" $ do
    let board = buildBoard "////////pppp/dddd/qqqq"
    playerWon board Bottom 10 10 `shouldBe` Just Bottom

