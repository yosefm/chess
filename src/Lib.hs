{-# LANGUAGE TupleSections #-}

module Lib (
    Side(..), Piece(..), Square(..), Board
  , otherSide
  , startPositions, startBoard, emptyBoard
  , squareTaken, validMoves, isChess
) where

import Data.Maybe (fromJust)
import Array2D (Arr2D(..), mkArr2D, Extents(..), Coords, (@), inBounds, merge)

data Side = White | Black 
    deriving (Eq, Ord)
data Piece = Peon | Rook | Knight | Bishop | Queen | King
    deriving (Eq, Ord)
data Square = EmptySq | Sq { sqSide :: Side, sqPiece :: Piece }
    deriving Eq

otherSide :: Side -> Side
otherSide White = Black
otherSide Black = White

squareTaken :: Square -> Bool
squareTaken EmptySq = False
squareTaken _ = True

peonStartRank :: Side -> Int
peonStartRank White = 1
peonStartRank Black = 6

-- Generate initial positions for all pieces as 
-- a replacements list for Arr2D (i.e. sparse representation)
startPositions :: [((Int, Int), Square)]
startPositions = 
    let piecesFirstRank = [Rook, Knight, Bishop, Queen, King, Bishop, Knight, Rook]
        squaresRank1 side = map (Sq side) piecesFirstRank
        rank n = map (n,) [0..7]
        
        replsRank1 = zip (rank 0) $ squaresRank1 White
        replsRank2 = map (,Sq White Peon) $ rank $ peonStartRank White
        replsRank7 = map (,Sq Black Peon) $ rank $ peonStartRank Black
        replsRank8 = zip (rank 7) $ squaresRank1 Black
        
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
        
        ray dr dc = [(r + dr*n, c + dc*n) | n <- [1..]]
        axisRays = [ray 0 1, ray 0 (-1), ray 1 0, ray (-1) 0]
        diagRays = [ray 1 1, ray 1 (-1), ray (-1) 1, ray (-1) (-1)]

        advanceValid crd' = inBounds (arrShape brd) crd' && not (squareTaken $ brd @ crd')
        advanceOrStrike side' crd' = inBounds (arrShape brd) crd' 
                            && (not (squareTaken $ brd @ crd') || sqSide (brd @ crd') /= side')
                        
        takeIfStrike _ [] = []
        takeIfStrike side' (mv:_) = [
               mv| inBounds (arrShape brd) mv && sqSide (brd @ mv) /= side'
            ]
                        
        toBlockOrStrike side' = (((++) <$> fst <*> takeIfStrike side' . snd) . span advanceValid) 

    in case originSq of 
        EmptySq -> []
        Sq side piece -> case piece of
            Peon -> let advance White = 1
                        advance Black = (-1)
                        secondAdvance = if r == peonStartRank side 
                            then [(r + (2*advance side), c)]
                            else []
                        advanceMoves = takeWhile advanceValid $ (r + advance side, c) : secondAdvance

                        strikeValid crd' = inBounds (arrShape brd) crd' 
                            && squareTaken (brd @ crd') 
                            && sqSide (brd @ crd') /= side
                        strikeMoves = filter strikeValid [(r + advance side, c - 1), (r + advance side, c + 1)]

                    in advanceMoves ++ strikeMoves
            
            Rook -> axisRays >>= toBlockOrStrike side
            Bishop -> diagRays >>= toBlockOrStrike side
            Queen -> axisRays ++ diagRays >>= toBlockOrStrike side

            Knight -> filter (advanceOrStrike side) [(r + dr, c + dc) | 
                        (dr,dc) <- [(2,1), (2,-1), (-2,1), (-2,-1), (1,2), (1,-2), (-1,2), (-1,-2)]]

            King -> filter (advanceOrStrike side) [(r + dr, c + dc) | dr <- [-1..1], dc <-[-1..1]]

-- is the king of the given side threatened?
-- This can be improved by only checking last-moved-to coordinate,
-- with cooperation from driver code.
isChess :: Board -> Side -> Bool
isChess brd side = any isKing $ boardCoords >>= validMoves brd
    where boardCoords = [(row, col) | row <- [0..7], col <- [0..7]]
          isKing crd = brd @ crd == Sq side King
