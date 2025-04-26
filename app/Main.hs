{-# LANGUAGE TupleSections #-}

module Main (main) where

import qualified Data.Map as M
import Data.Maybe (fromJust)

import System.Exit (exitFailure)
import Codec.BMP (BMP, readBMP)
import Graphics.Gloss

import Lib
import Array2D (Arr2D, mkArr2D, Extents(..), merge, (@))

data BoardVisualProps = BoardVP {
    boardSidePix :: Int
  , pieceImages :: BMP
}

-- I mean, I could do O(n) lookup with Ix and Vector,
-- but it would look ugly and there's no call for performance here.
pieceRects :: M.Map (Side,Piece) Rectangle
pieceRects = M.fromList [
    ((White, Peon), Rectangle (0,170) (55,75))
  , ((White, Rook), Rectangle (67,165) (56,80))
  , ((White, Knight), Rectangle  (141,154) (66,91))
  , ((White, Bishop), Rectangle  (221,132) (56,113))
  , ((White, Queen), Rectangle (296,111) (61,134))
  , ((White, King), Rectangle (376,97) (55,148))
  , ((Black, Peon), Rectangle (376,0) (55,76))
  , ((Black, Rook), Rectangle (308,0) (56,80))
  , ((Black, Knight), Rectangle (224,0) (66,91))
  , ((Black, Bishop), Rectangle (154,0) (56,113))
  , ((Black, Queen), Rectangle (74,0) (61,134))
  , ((Black, King), Rectangle (0,0) (55,148))
  ]

maxPieceHeight :: Int
maxPieceHeight = maximum $ map (snd . rectSize) $ M.elems pieceRects

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

square :: Int -> Picture
square sidePix = Polygon [(0,0), (side,0), (side,side), (0,side)]
    where side = fromIntegral sidePix

board :: Int -> Picture
board sidePix = 
    let squareSide = sidePix `div` 8
        halfBoard = fromIntegral (sidePix `div` 2)
        boardCoords = [(row, col) | row <- [0..7], col <- [0..7]]
        squares = Pictures $ map makeSqr boardCoords

        makeSqr (r,c) = Translate 
            (fromIntegral $ c*squareSide) (fromIntegral $ r*squareSide) 
            $ Color (if ((r + c) `mod` 2) == 0 then black else white)
            $ square squareSide
        
    in Translate (-halfBoard) (-halfBoard) $ Color black $ squares

-- For now just white peon. Later: indicate which piece and side.
showPiece :: BoardVisualProps -> (Int,Int) -> Square -> Picture
showPiece _ _ EmptySq = Blank

showPiece (BoardVP boardSide image) (r,c) (Sq side piece) = 
    let squareSide = fromIntegral $ boardSide `div` 8
        bmpData = bitmapDataOfBMP image
        (tw,th) = bitmapSize bmpData
        rect = fromJust $ M.lookup (side,piece) pieceRects

        rowCoord = (fromIntegral r + 0.5)*squareSide - fromIntegral (boardSide `div` 2)
        colCoord = (fromIntegral c + 0.5)*squareSide - fromIntegral (boardSide `div` 2)

        scale = squareSide / fromIntegral maxPieceHeight

    in Translate colCoord rowCoord $ Scale scale scale $ BitmapSection rect bmpData

showBoard :: BoardVisualProps -> Board -> Picture
showBoard bvp board = 
    let boardCoords = [(row, col) | row <- [0..7], col <- [0..7]]
    in mconcat $ map (\at -> showPiece bvp at $ board @ at) boardCoords

loadPiecesOrDie :: IO BMP
loadPiecesOrDie = do
    peonImageLoadRes <- readBMP "data/akiross-Chess-Set.bmp"
    case peonImageLoadRes of 
        Left e -> do
            putStrLn $ show e
            exitFailure
        Right bmp -> return bmp

main :: IO ()
main = do 
    piecesImage <- loadPiecesOrDie

    let boardSide = 600
        boardVis = BoardVP boardSide piecesImage
        layedOutBoard = board boardSide <> showBoard boardVis startBoard
    
    display (InWindow "Test Gloss" (boardSide,boardSide) (10,10)) white $ layedOutBoard

