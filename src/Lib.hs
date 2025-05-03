{-# LANGUAGE TupleSections #-}

module Lib (
    Side(..), Piece(..), Square(..), Board
  , startPositions, startBoard, emptyBoard
  , squareTaken, validMoves
) where

import Data.Maybe (fromJust)
import Array2D (Arr2D(..), mkArr2D, Extents(..), Coords, (@), inBounds, merge)

data Side = White | Black 
    deriving (Eq, Ord)
data Piece = Peon | Rook | Knight | Bishop | Queen | King
    deriving (Eq, Ord)
data Square = EmptySq | Sq { sqSide :: Side, sqPiece :: Piece }

squareTaken :: Square -> Bool
squareTaken EmptySq = False
squareTaken _ = True

-- Generate initial positions for all pieces as 
-- a replacements list for Arr2D (i.e. sparse representation)
startPositions :: [((Int, Int), Square)]
startPositions = 
    let piecesFirstRank = [Rook, Knight, Bishop, Queen, King, Bishop, Knight, Rook]
        squaresRank1 side = map (Sq side) piecesFirstRank

        rank1 = map (0,) $ enumFromTo 0 7
        replsRank1 = zip rank1 $ squaresRank1 White

        rank2 = map (1,) $ enumFromTo 0 7
        replsRank2 = map (,Sq White Peon) rank2

        rank7 = map (6,) $ enumFromTo 0 7
        replsRank7 = map (,Sq Black Peon) rank7

        rank8 = map (7,) $ enumFromTo 0 7
        replsRank8 = zip rank8 $ squaresRank1 Black
        
    in concat [replsRank1, replsRank2, replsRank7, replsRank8]

type Board = Arr2D Square

emptyBoard :: Board
emptyBoard = fromJust $ mkArr2D (Ex 8 8) $ replicate 64 EmptySq

startBoard :: Board
startBoard = fromJust $ merge emptyBoard startPositions

-- assume coords are in bounds.
validMoves :: Board -> Coords -> [Coords]
validMoves  brd crd@(r,c) = 
    let originSq = brd @ crd
    in case originSq of 
        EmptySq -> []
        Sq side piece -> case piece of
            Peon -> let advance White = 1
                        advance Black = (-1)
                        advanceValid crd' = inBounds (arrShape brd) crd' && not (squareTaken $ brd @ crd')
                        advanceMoves = takeWhile advanceValid [(r + advance side, c), (r + (2*advance side), c)]

                        strikeValid crd' = inBounds (arrShape brd) crd' 
                            && squareTaken (brd @ crd') 
                            && sqSide (brd @ crd') /= side
                        strikeMoves = filter strikeValid [(r + advance side, c - 1), (r + advance side, c + 1)]
                    in advanceMoves ++ strikeMoves
            _ -> []
