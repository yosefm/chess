import qualified Data.Set as Set
import Data.Maybe (fromJust)
import Array2D (merge)

import Lib (validMoves, isChess
    , startBoard, emptyBoard, Piece (..), Side(..), Square(..))
import Test.Hspec

equalAsSets :: Ord a => [a] -> [a] -> Bool
equalAsSets a b = Set.fromList a == Set.fromList b

main :: IO ()
main = hspec $ do
    describe "Valid moves" $ do
        it "lets white peon move forward to empty squares" $ do
            validMoves startBoard (1,1) `shouldSatisfy` equalAsSets [(2,1), (3,1)]
        it "lets black peon move forward to empty squares" $ do
            validMoves startBoard (6,1) `shouldSatisfy` equalAsSets [(4,1), (5,1)]
        
        it "allows only one peon advance from non-start rank" $ do
            let brd = fromJust $ merge emptyBoard [
                    ((2,1), Sq White Peon)
                  , ((5,0), Sq Black Peon)
                  ]
            validMoves brd (2,1) ++ validMoves brd (5,0) `shouldBe` [(3,1), (4,0)]

        it "lets white peon strike diagonally" $ do
            let brd = fromJust $ merge emptyBoard [
                    ((1,1), Sq White Peon)
                  , ((2,0), Sq Black Peon)
                  , ((2,1), Sq Black Peon)  -- blocked forward moves
                  , ((2,2), Sq Black Peon)
                  ]
            validMoves brd (1,1) `shouldSatisfy` equalAsSets [(2,0), (2,2)]
        
        it "lets rook move to board ends except blocking and striking" $ do
            let brd = fromJust $ merge emptyBoard [
                    ((2,2), Sq White Rook)
                  , ((2,4), Sq White Peon) -- blocked right moves by same side
                  , ((4,2), Sq Black Peon) -- blocked up moves by strike
                  ]
            validMoves brd (2,2) `shouldSatisfy` equalAsSets 
                [(2,3), (2,1), (2,0), (1,2), (0,2), (3,2), (4,2)]
        
        it "lets bishop move diagonally except blocking and striking"$ do
            let brd = fromJust $ merge emptyBoard [
                    ((2,2), Sq White Bishop)
                  , ((4,4), Sq White Peon) -- blocked right moves by same side
                  , ((4,0), Sq Black Peon) -- blocked up moves by strike
                  ]
            validMoves brd (2,2) `shouldSatisfy` equalAsSets 
                [(3,3), (3,1), (4,0), (1,1), (1,3), (0,0), (0,4)]
        
        it "lets queen move down all rays, sans block/strike" $ do
            -- just a combination of the rook and bishop test cases
            let brd = fromJust $ merge emptyBoard [
                    ((2,2), Sq White Queen)
                  , ((2,4), Sq White Peon) -- blocked right moves by same side
                  , ((4,2), Sq Black Peon) -- blocked up moves by strike
                  , ((4,4), Sq White Peon) -- blocked right moves by same side
                  , ((4,0), Sq Black Peon) -- blocked up moves by strike
                  ]
            validMoves brd (2,2) `shouldSatisfy` equalAsSets 
                [(2,3), (2,1), (2,0), (1,2), (0,2), (3,2), (4,2), (3,3), (3,1), (4,0), (1,1), (1,3), (0,0), (0,4)]
        
        it "generates knight moves except on own pieces" $ do
            let brd = fromJust $ merge emptyBoard [
                    ((4,4), Sq White Knight)
                  , ((6,5), Sq White Peon) -- blocked right moves by same side
                  , ((6,-5), Sq Black Peon) -- blocked up moves by strike
                  ]
            validMoves brd (4,4) `shouldSatisfy` equalAsSets 
                [(6,3), (2,5), (2,3), (5,2), (5,6), (3,2), (3,6)]
        
        it "generates correct king moves" $ do
            let brd = fromJust $ merge emptyBoard [
                    ((2,2), Sq White King)
                  , ((3,3), Sq White Peon) -- blocked right moves by same side
                  , ((2,3), Sq Black Peon) -- blocked up moves by strike
                  ]
            validMoves brd (2,2) `shouldSatisfy` equalAsSets 
                [(3,1), (3,2), (2,1), (2,3), (1,1), (1,2), (1,3)]
    
    describe "Board States" $ do
        let whiteChessedBrd = fromJust $ merge emptyBoard [
                    ((2,2), Sq White King)
                  , ((3,3), Sq Black Peon) -- puts king in chess
                  ]
        
        it "identifies chess state" $ do
            isChess whiteChessedBrd White `shouldBe` True
        
        it "respects side in chess query" $ do
            isChess whiteChessedBrd Black `shouldBe` False
        
        it "identifies that there is no chess" $ do
            isChess startBoard White `shouldBe` False
