-- #############################################################################
-- ###### TESTS                                                       ##########
-- #############################################################################

import Test.Hspec

import Board (validateFEN, buildBoard, buildFEN,
              Player(Top, Bottom),
              Cell(Empty, Pawn, Drone, Queen),
              Pos(Pos))

import Logic (Move(Move), pawnMoves, droneMoves, queenMoves, makeMove, playerWon)

main :: IO ()
main = hspec $ do
  describe "Marsian Chess Tests" $ do
    it "placeholder test" $ do
      True `shouldBe` True
