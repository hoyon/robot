module Game where

data Grid = Grid { gridX :: Int, gridY :: Int }
  deriving (Eq, Show)

data Robot = Robot 
              { robotX :: Int
              , robotY :: Int
              , robotOrientation :: Orientation
              , dead :: Bool }
  deriving (Eq, Show)

data Orientation = North | East | South | West
  deriving (Eq, Show)

data Action = F | L | R
  deriving (Eq, Show)

play :: Grid -> Robot -> [Action] -> Robot
play grid robot actions =
    foldl (step grid) robot actions

step :: Grid -> Robot -> Action -> Robot
step grid robot@Robot{dead=False} action =
    let
      newRobot = performAction action robot
    in
      if isLost newRobot grid
        then robot { dead = True }
        else newRobot

step _ position _ =
    position

performAction :: Action -> Robot -> Robot
performAction F pos =
    case pos of
      Robot x y North _ -> pos { robotY = y + 1 }
      Robot x y East _ -> pos { robotX = x + 1 }
      Robot x y South _ -> pos { robotY = y - 1 }
      Robot x y West _ -> pos { robotX = x - 1 }

performAction L pos =
    case robotOrientation pos of
      North -> pos { robotOrientation = West }
      East -> pos { robotOrientation = North }
      South -> pos { robotOrientation = East }
      West -> pos { robotOrientation = South }

performAction R pos =
    case robotOrientation pos of
      North -> pos { robotOrientation = East }
      East -> pos { robotOrientation = South }
      South -> pos { robotOrientation = West }
      West -> pos { robotOrientation = North }

isLost :: Robot -> Grid -> Bool
isLost robot grid =
  robotX robot > gridX grid ||
  robotY robot > gridY grid ||
  robotX robot < 0 ||
  robotY robot < 0
