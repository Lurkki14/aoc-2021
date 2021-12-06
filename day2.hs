import Data.Maybe
import Text.Read

data Direction = Up | Down | Forward deriving (Show)
data Move = Move Direction Int deriving (Show)
newtype Depth = Depth Int deriving (Show)
newtype Distance = Distance Int deriving (Show)
data Coordinates = Coordinates Depth Distance deriving (Show)

parseDirection "forward" = Just Forward
parseDirection "up" = Just Up
parseDirection "down" = Just Down
parseDirection _ = Nothing

parseMove :: String -> Maybe Move
parseMove line = parseWords (words line) where
  parseWords [dir, n] = do
    d <- parseDirection dir
    m <- readMaybe n
    pure $ Move d m
  parseWords _ = Nothing

addMove :: Move -> Coordinates -> Coordinates
addMove (Move Up x) (Coordinates (Depth y) dist) = Coordinates (Depth (y - x)) dist
addMove (Move Down x) (Coordinates (Depth y) dist) = Coordinates (Depth (y + x)) dist
addMove (Move Forward x) (Coordinates depth (Distance z)) = Coordinates depth (Distance (z + x))

originCoords = Coordinates (Depth 0) (Distance 0)

main = do
  contents <- readFile "input2"
  let lines' = lines contents
  let moves = catMaybes $ fmap parseMove lines'
  let destination = foldr addMove originCoords moves
  print destination
