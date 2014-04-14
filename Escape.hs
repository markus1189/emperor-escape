{-# LANGUAGE TemplateHaskell #-}
import           Control.Lens (view, use, each, filtered, findOf, to, ix)
import           Control.Lens.Operators
import           Control.Lens.TH (makeLenses)
import           Control.Monad (unless)
import           Data.Attoparsec.Text (parseOnly)
import qualified Data.Map as Map
import qualified Data.List as List
import           Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Text.IO as TextIO

import           Parse
import           Types

data Explore = Explore { _current :: Room
                       , _toExplore :: [Room]
                       , _playerHp :: Int
                       , _exploredRooms :: Set Room
                       }
makeLenses ''Explore

main :: IO ()
main = do
  contents <- TextIO.getContents
  case parseOnly testCases contents of
    Left e -> print e
    Right caves -> mapM_ (print . tryEscape) caves

startRoom :: Cave -> Maybe Room
startRoom = findOf (corridors . to Map.keys . each) ((==1). view roomNumber)

tryEscape :: Cave -> Outcome
tryEscape c = case start of
                   Nothing -> Invalid "No start room (id=1) found."
                   Just r -> searchWayOut c (Explore r (c ^. corridors . ix r) 0 Set.empty)
  where start = startRoom c

searchWayOut :: Cave -> Explore -> Outcome
searchWayOut c e =
  if exitReached c e
  then Escaped
  else case chooseNextRoom e of
    Nothing -> Trapped
    Just r -> searchWayOut c (exploreRoom r c e)

exitReached :: Cave -> Explore -> Bool
exitReached c e = c ^. exitRoom == e ^. current

exploreRoom :: Room -> Cave -> Explore -> Explore
exploreRoom r c e = e &~ do
  toExplore %= List.delete r
  toExplore <>= c ^. corridors . ix r
  current .= r
  seen <- use exploredRooms
  unless (Set.member r seen) $ playerHp += r ^. roomHp
  exploredRooms %= Set.insert r

safeRoom :: Room -> Explore -> Bool
safeRoom r e = hpGainForRoom r e >= 0

hpGainForRoom :: Room -> Explore -> Int
hpGainForRoom r e = view playerHp e + view roomHp r

chooseNextRoom :: Explore -> Maybe Room
chooseNextRoom e = e ^? toExplore . each . filtered (`safeRoom` e)
