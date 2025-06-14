module Main (main) where

import qualified Data.Map as M
import Data.Maybe (fromJust, catMaybes)

import System.Exit (exitFailure)
import Codec.BMP (BMP, readBMP)

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Interact

import Lib
import Array2D ((@), Extents(..), Coords, inBounds, merge)

data BoardVisualProps = BoardVP {
    boardSidePix :: Int
  , pieceImages :: M.Map (Side,Piece) Picture
  , statusLineThicknessPix :: Int
}

-- I mean, I could do O(1) lookup with Ix and Vector,
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
            $ Color (if even (r + c) then black else white)
            $ square squareSide
        
    in Translate (-halfBoard) (-halfBoard) $ Color black squares

-- For now just white peon. Later: indicate which piece and side.
showPiece :: BoardVisualProps -> Coords -> Square -> Picture
showPiece _ _ EmptySq = Blank

showPiece (BoardVP boardSide images _) (r,c) (Sq side piece) = 
    let squareSide = fromIntegral $ boardSide `div` 8
        rowCoord = (fromIntegral r + 0.5)*squareSide - fromIntegral (boardSide `div` 2)
        colCoord = (fromIntegral c + 0.5)*squareSide - fromIntegral (boardSide `div` 2)

        scale = squareSide / fromIntegral maxPieceHeight

    in Translate colCoord rowCoord $ 
       Scale scale scale $ 
       fromJust $ M.lookup (side,piece) images

showBoard :: BoardVisualProps -> Board -> Picture
showBoard bvp board = 
    let boardCoords = [(row, col) | row <- [0..7], col <- [0..7]]
    in mconcat $ map (\at -> showPiece bvp at $ board @ at) boardCoords

showSelection :: BoardVisualProps -> Coords -> Picture
showSelection (BoardVP boardSide _ _) (r,c) = 
    let squareSide = boardSide `div` 8
        rowCoord = fromIntegral $ r*squareSide - boardSide `div` 2
        colCoord = fromIntegral $ c*squareSide - boardSide `div` 2
    
    in Translate colCoord rowCoord $ Color red $ square squareSide

showValidTarget :: BoardVisualProps -> Coords -> Picture
showValidTarget (BoardVP boardSide _ _) (r,c) = 
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

showStatus :: Game -> Picture
showStatus g = 
    let bvp = boardVis g
        statusPix = fromIntegral $ statusLineThicknessPix bvp
        vectorLineHeight = 5
        textScale = vectorLineHeight/statusPix
        halfBoard = fromIntegral $ boardSidePix bvp `div` 2
    in Translate (-halfBoard) (vectorLineHeight - statusPix - halfBoard) $
       Scale textScale textScale $
       Text $ show (toPlay g) ++ " to play."

showGame :: Game -> Picture
showGame g = 
    let bvp = boardVis g
        statusPix = statusLineThicknessPix bvp
    in Translate 0 ((fromIntegral statusPix)/2) $ (mconcat (catMaybes [
        Just (board $ boardSidePix bvp)
      , showSelection bvp <$> selected g
      ] ++ map (showValidTarget bvp) (maybe [] (validMoves (boardState g)) (selected g)))
    <> showBoard bvp (boardState g))
    <> showStatus g

-- convert mouse click position to square coordinates
-- Nothing if the coordinates are out of board.
clickToBoardCoords :: Game -> (Float,Float) -> Maybe Coords
clickToBoardCoords g (x,y) = 
    let boardSide = boardSidePix $ boardVis g
        halfBoard = fromIntegral $ boardSide `div` 2
        squareSide = fromIntegral $ boardSide `div` 8
        crd = (truncate ((y + halfBoard)/squareSide),  truncate ((x + halfBoard)/squareSide))
    in if inBounds (Ex 8 8) crd then Just crd else Nothing

-- the selected square is empty or the piece is of
-- the wrong side 
selectedPiece :: Game -> (Float,Float) -> Maybe Coords
selectedPiece g clickPos = 
    clickToBoardCoords g clickPos >>= selectableCrd
    where 
      selectableCrd crd' = 
        let square = boardState g @ crd'
            valid = squareTaken square && sqSide square == toPlay g
        in if valid then Just crd' else Nothing 

eventCallback :: Event -> Game -> Game
eventCallback (EventKey (MouseButton LeftButton) Down _ clickPos) g = 
    let vms = validMoves (boardState g)

    in case selected g of
        Nothing -> g{selected = selectedPiece g clickPos}
        Just origCrd -> case clickToBoardCoords g clickPos of 
            Nothing -> g
            Just crd -> if elem crd (vms origCrd)
                        then g{selected = Nothing, 
                               boardState = fromJust $ merge (boardState g) [
                                  (origCrd, EmptySq), (crd, boardState g @ origCrd)
                                ],
                               toPlay = otherSide $ toPlay g
                              }
                        else g{selected = Nothing}

eventCallback _ g = g

loadPiecesOrDie :: IO BMP
loadPiecesOrDie = do
    peonImageLoadRes <- readBMP "data/akiross-Chess-Set.bmp"
    case peonImageLoadRes of 
        Left e -> do
            print e
            exitFailure
        Right bmp -> return bmp

main :: IO ()
main = do 
    piecesImage <- loadPiecesOrDie

    let boardSide = 600
        extractedPieces = M.map (flip BitmapSection $ bitmapDataOfBMP piecesImage) pieceRects
        gameInitState = Game startBoard (BoardVP boardSide extractedPieces 30) White Nothing
        
    play 
        (InWindow "StupidChess" (boardSide,boardSide + 30) (10,10)) 
        white 30 gameInitState 
        showGame eventCallback (\_ g -> g)
    