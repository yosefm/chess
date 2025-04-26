{-# LANGUAGE TupleSections #-}

module Lib (
    Side(..), Piece(..), Square(..), Board
  , startPositions, startBoard
) where

import Data.Maybe (fromJust)
import Array2D (Arr2D, mkArr2D, Extents(..), merge)

data Side = White | Black 
    deriving (Eq, Ord)
data Piece = Peon | Rook | Knight | Bishop | Queen | King
    deriving (Eq, Ord)
data Square = EmptySq | Sq Side Piece


-- Generate initial positions for all pieces as 
-- a replacements list for Arr2D (i.e. sparse representation)
startPositions :: [((Int, Int), Square)]
startPositions = 
    let piecesFirstRank = [Rook, Knight, Bishop, Queen, King, Bishop, Knight, Rook]
        squaresRank1 side = map (Sq side) piecesFirstRank

        rank1 = map (0,) $ enumFromTo 0 7
        replsRank1 = zip rank1 $ squaresRank1 White

        rank2 = map (1,) $ enumFromTo 0 7
        replsRank2 = zip rank2 $ map (Sq White) $ repeat Peon

        rank7 = map (6,) $ enumFromTo 0 7
        replsRank7 = zip rank7 $ map (Sq Black) $ repeat Peon

        rank8 = map (7,) $ enumFromTo 0 7
        replsRank8 = zip rank8 $ squaresRank1 Black
        
    in concat [replsRank1, replsRank2, replsRank7, replsRank8]

type Board = Arr2D Square

startBoard :: Board
startBoard = fromJust $ merge emptyBoard startPositions
    where emptyBoard = fromJust $ mkArr2D (Ex 8 8) $ take 64 $ repeat EmptySq

