module Main (main) where

import System.Exit (exitFailure)
import Codec.BMP (BMP, readBMP)
import Graphics.Gloss

import Lib

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
showPiece :: Int -> BMP -> (Int,Int) -> Picture
showPiece boardSide image (r,c) = 
    let squareSide = fromIntegral $ boardSide `div` 8
        bmpData = bitmapDataOfBMP image
        (tw,th) = bitmapSize bmpData
        (pw,ph) = (55,75)
        rowCoord = (fromIntegral r + 0.5)*squareSide - fromIntegral (boardSide `div` 2)
        colCoord = (fromIntegral c + 0.5)*squareSide - fromIntegral (boardSide `div` 2)
    in Translate colCoord rowCoord $ BitmapSection (Rectangle (0,th - ph) (pw,ph)) bmpData

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
        empty = board boardSide
        withPeon = empty <> showPiece boardSide piecesImage (0, 1)
        layedOutBoard = withPeon
    
    display (InWindow "Test Gloss" (600,600) (10,10)) white $ layedOutBoard

