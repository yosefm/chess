module Main (main) where

import qualified Data.Map as M
import Data.Maybe (fromJust)

import System.Exit (exitFailure)
import Codec.BMP (BMP, readBMP)
import Graphics.Gloss

import Lib

data BoardVisualProps = BoardVP {
    boardSidePix :: Int
  , pieceImages :: BMP
}

-- I mean, I could do O(n) lookup with Ix and Vector,
-- but it would look ugly and there's no call for performance here.
pieceRects :: M.Map (Side,Piece) Rectangle
pieceRects = M.fromList [
    ((White, Peon), Rectangle (0,170) (55,75))
  , ((White, Rook), Rectangle (65,164) (55,81))
  , ((White, Knight), Rectangle (135,155) (76,91))
  ]

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

    in Translate colCoord rowCoord $ BitmapSection rect bmpData

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
        empty = board boardSide
        layedOutBoard = empty 
            <> showPiece boardVis (1, 0) (Sq White Peon)
            <> showPiece boardVis (0, 0) (Sq White Rook)
            <> showPiece boardVis (0, 1) (Sq White Knight)
    
    display (InWindow "Test Gloss" (600,600) (10,10)) white $ layedOutBoard

