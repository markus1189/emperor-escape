module Parse ( testCases
             ) where

import           Control.Applicative ((<$>), (<*), (<*>))
import           Control.Lens (each, both, ix)
import           Control.Lens.Operators
import           Control.Monad (replicateM)
import           Data.Attoparsec.Text
import           Data.Map (Map)
import qualified Data.Map as Map

import qualified Types

testCases :: Parser [Types.Cave]
testCases = do
  n <- numberOfTestCases <* endOfLine
  replicateM n testCase

numberOfTestCases :: Parser Int
numberOfTestCases = signed decimal

testCase :: Parser Types.Cave
testCase = do
  (numRooms,exitNum) <- caveHeader
  numToRoom <- buildMap <$> rooms numRooms
  cs <- corridors (numRooms - 1)
  let corridorMap = reifyCorridors cs numToRoom
      exitRoom = numToRoom ^? ix exitNum
  case exitRoom of
    Just er -> return $ Types.Cave er corridorMap
    Nothing -> fail "No exit room."

caveHeader :: Parser (Int,Int)
caveHeader = (,) <$> decimal <* space <*> decimal <* endOfLine

rooms :: Int -> Parser [Types.Room]
rooms n = do
  hpsInit <- replicateM (n-1) (room <* space)
  hpsLast <- room <* endOfLine
  return $ zipWith Types.Room [1..] (hpsInit |> hpsLast)

room :: Parser Int
room = signed decimal

corridors :: Int -> Parser [(Int,Int)]
corridors 0 = return []
corridors n = (:) <$> corridor <*> corridors (n-1)

corridor :: Parser (Int, Int)
corridor = ((,) <$> decimal <* space <*> decimal) <* endOfLine

buildMap :: [Types.Room] -> Map Int Types.Room
buildMap = Map.fromList . map (\r -> (r ^. Types.roomNumber,r))

reifyCorridors :: [(Int,Int)] -> Map Int Types.Room -> Map Types.Room [Types.Room]
reifyCorridors edges numToRoom =
  foldr (\(from,to) -> Map.insertWith (++) from [to]) Map.empty roomEdges
  where
    roomEdges :: [(Types.Room,Types.Room)]
    roomEdges = justPairs (edges & each . both %~ flip Map.lookup numToRoom)

justPairs :: [(Maybe a, Maybe b)] -> [(a,b)]
justPairs [] = []
justPairs ((Just x, Just y):xs) = (x,y) : justPairs xs
justPairs (_:xs) = justPairs xs
