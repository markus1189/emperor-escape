{-# LANGUAGE TemplateHaskell #-}
module Types ( Room (Room)
             , roomNumber
             , roomHp

             , Cave (Cave)
             , exitRoom
             , corridors

             , Outcome (..)
             ) where

import Control.Lens.TH (makeLenses)
import Data.Map (Map)

data Room = Room { _roomNumber :: Int
                 , _roomHp :: Int
                 } deriving (Show,Ord,Eq)
makeLenses ''Room

data Cave = Cave { _exitRoom :: Room
                 , _corridors :: Map Room [Room]
                 } deriving (Show,Ord,Eq)
makeLenses ''Cave

data Outcome = Escaped | Trapped | Invalid String deriving (Show,Eq)
