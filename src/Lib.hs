{-# LANGUAGE TupleSections #-}

module Lib (
    Side(..), Piece(..), Square(..), Board, ThreatLevel(..)
  , otherSide
  , startPositions, startBoard, emptyBoard
  , squareTaken, validMoves, isCheck, threatLevel
) where

import Data.Maybe (fromJust)
import Array2D (Arr2D(..), mkArr2D, Extents(..), Coords, (@), inBounds, merge)
import qualified Data.Set as Set

data Side = White | Black 
    deriving (Eq, Ord, Show)
data Piece = Peon | Rook | Knight | Bishop | Queen | King
    deriving (Eq, Ord)
data Square = EmptySq | Sq { sqSide :: Side, sqPiece :: Piece }
    deriving Eq

data ThreatLevel = Safe | Check | Mate deriving (Eq, Show)

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

peonAdvance :: Side -> Int
peonAdvance White = 1
peonAdvance Black = (-1)

-- Which coords are under threat from given piece?
threatCoords :: Board -> Coords -> [Coords]
threatCoords brd crd@(r,c) = 
    let originSq = brd @ crd
        free side' crd' = not (squareTaken (brd @ crd')) || sqSide (brd @ crd') /= side'
        inside = inBounds (arrShape brd)

        ray dr dc = [(r + dr*n, c + dc*n) | n <- [1..]]
        axisRays = [ray 0 1, ray 0 (-1), ray 1 0, ray (-1) 0]
        diagRays = [ray 1 1, ray 1 (-1), ray (-1) 1, ray (-1) (-1)]

        advanceValid crd' = inside crd' && not (squareTaken $ brd @ crd')
        advanceOrStrike side' crd' = inside crd' 
                            && (not (squareTaken $ brd @ crd') || sqSide (brd @ crd') /= side')
                        
        takeIfStrike _ [] = []
        takeIfStrike side' (mv:_) = [
               mv| inside mv && sqSide (brd @ mv) /= side'
            ]
                        
        toBlockOrStrike side' = (((++) <$> fst <*> takeIfStrike side' . snd) . span advanceValid) 

    in case originSq of 
        EmptySq -> []
        Sq side piece -> case piece of
            Peon -> let advance = peonAdvance side
                        valid = (&&) <$> free side <*> inside
                    in filter valid [(r + advance, c - 1), (r + advance, c + 1)]

            Rook -> axisRays >>= toBlockOrStrike side
            Bishop -> diagRays >>= toBlockOrStrike side
            Queen -> axisRays ++ diagRays >>= toBlockOrStrike side

            Knight -> filter (advanceOrStrike side) [(r + dr, c + dc) | 
                        (dr,dc) <- [(2,1), (2,-1), (-2,1), (-2,-1), (1,2), (1,-2), (-1,2), (-1,-2)]]
            
            King -> filter (advanceOrStrike side) [(r + dr, c + dc) | dr <- [-1..1], dc <-[-1..1]]

validMoves :: Board -> Coords -> [Coords]
validMoves  brd crd@(r,c) = 
    let originSq = brd @ crd
        enemyAt side' crd'= 
            squareTaken (brd @ crd')
            && sqSide (brd @ crd') /= side'

    in case originSq of 
        EmptySq -> []
        Sq side piece -> case piece of
            Peon -> let advance = peonAdvance side
                        secondAdvance = if r == peonStartRank side 
                            then [(r + 2*advance, c)]
                            else []
                        
                        advanceValid crd' = inBounds (arrShape brd) crd' && not (squareTaken $ brd @ crd')
                        advanceMoves = takeWhile advanceValid $ (r + advance, c) : secondAdvance
                        strikeMoves = filter (enemyAt side) $ threatCoords brd crd

                    in advanceMoves ++ strikeMoves
            
            King -> let unfiltered = threatCoords brd crd 
                        boardCoords = [(row, col) | row <- [0..7], col <- [0..7]]
                        enemyPieces = filter (enemyAt side) boardCoords
                        threats = Set.fromList $ concat $ map (threatCoords brd) enemyPieces
                    in filter (flip Set.notMember threats) unfiltered

            _ -> threatCoords brd crd 

checkedKing :: Board -> Side -> Maybe Coords
checkedKing brd side
    | whereChecked == []  = Nothing
    | otherwise           = Just $ head whereChecked

  where 
    boardCoords = [(row, col) | row <- [0..7], col <- [0..7]]
    isKing crd = brd @ crd == Sq side King

    whereChecked = filter isKing $ boardCoords >>= threatCoords brd

threatLevel :: Board -> Side -> ThreatLevel
threatLevel brd side = case checkedKing brd side of
    Nothing -> Safe
    Just crd  
      | length (validMoves brd crd) > 0 -> Check
      | otherwise -> Mate

-- is the king of the given side threatened?
-- This can be improved by only checking last-moved-to coordinate,
-- with cooperation from driver code.
isCheck :: Board -> Side -> Bool
isCheck b = (== Check) . threatLevel b
