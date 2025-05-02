module Main (main) where

import qualified Data.Map as M
import Data.Maybe (fromJust, catMaybes)

import System.Exit (exitFailure)
import Codec.BMP (BMP, readBMP)

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Interact

import Lib
import Array2D ((@), Coords)

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
showPiece :: BoardVisualProps -> Coords -> Square -> Picture
showPiece _ _ EmptySq = Blank

showPiece (BoardVP boardSide image) (r,c) (Sq side piece) = 
    let squareSide = fromIntegral $ boardSide `div` 8
        bmpData = bitmapDataOfBMP image
        rect = fromJust $ M.lookup (side,piece) pieceRects

        rowCoord = (fromIntegral r + 0.5)*squareSide - fromIntegral (boardSide `div` 2)
        colCoord = (fromIntegral c + 0.5)*squareSide - fromIntegral (boardSide `div` 2)

        scale = squareSide / fromIntegral maxPieceHeight

    in Translate colCoord rowCoord $ Scale scale scale $ BitmapSection rect bmpData

showBoard :: BoardVisualProps -> Board -> Picture
showBoard bvp board = 
    let boardCoords = [(row, col) | row <- [0..7], col <- [0..7]]
    in mconcat $ map (\at -> showPiece bvp at $ board @ at) boardCoords

showSelection :: BoardVisualProps -> Coords -> Picture
showSelection (BoardVP boardSide _) (r,c) = 
    let squareSide = boardSide `div` 8
        rowCoord = fromIntegral $ r*squareSide - boardSide `div` 2
        colCoord = fromIntegral $ c*squareSide - boardSide `div` 2
    
    in Translate colCoord rowCoord $ Color red $ square squareSide

showValidTarget :: BoardVisualProps -> Coords -> Picture
showValidTarget (BoardVP boardSide _) (r,c) = 
    let squareSide = fromIntegral $ boardSide `div` 8
        rowCoord = (fromIntegral r + 0.5)*squareSide - fromIntegral (boardSide `div` 2)
        colCoord = (fromIntegral c + 0.5)*squareSide - fromIntegral (boardSide `div` 2)
    
    in Translate colCoord rowCoord $ Color red $ Circle $ 0.45*squareSide

data Game = Game {
    boardState :: Board
  , boardVis :: BoardVisualProps
  , toPlay :: Side
  , selected :: Maybe Coords
}

showGame :: Game -> Picture
showGame g = 
    let bvp = boardVis g
    in mconcat $ catMaybes [
        Just (board $ boardSidePix bvp)
      , showSelection bvp <$> selected g
      , Just (showBoard bvp $ boardState g)
      ]

eventCallback :: Event -> Game -> Game
eventCallback (EventKey (MouseButton LeftButton) Down _ _) g = 
    case selected g of
        Nothing -> g{selected = Just (1,0)}
        Just _ -> g{selected = Nothing}

eventCallback _ g = g

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
        gameInitState = Game startBoard (BoardVP boardSide piecesImage) White Nothing
        
    play 
        (InWindow "StupidChess" (boardSide,boardSide) (10,10)) 
        white 30 gameInitState 
        showGame eventCallback (\t g -> g)
    