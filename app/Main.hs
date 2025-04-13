module Main (main) where

import Graphics.Gloss
import Graphics.Gloss.Data.Bitmap

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

main :: IO ()
main = do 
    peonImage <- loadBMP "data/peon.bmp"

    let empty = board 600
        withPeon = empty <> peonImage
        layedOutBoard = withPeon
    
    display (InWindow "Test Gloss" (600,600) (10,10)) white $ layedOutBoard

