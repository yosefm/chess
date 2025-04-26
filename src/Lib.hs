module Lib (
    Side(..), Piece(..), Square(..)
) where

data Side = White | Black 
    deriving (Eq, Ord)
data Piece = Peon | Rook | Knight | Bishop | Queen | King
    deriving (Eq, Ord)
data Square = EmptySq | Sq Side Piece
