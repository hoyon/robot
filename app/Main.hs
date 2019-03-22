module Main where

import Text.Parsec
import Text.Printf
import Game
import Parser
import System.IO

main :: IO ()
main = do
    gridStr <- getLine
    let grid = parseGrid gridStr
    run grid

parseGrid :: String -> Grid
parseGrid input =
    case parse grid "stdin" input of
      Left err -> error (show err)
      Right g -> g

run :: Grid -> IO ()
run grid = do
    line <- getLine
    let (robot, actions) = parseLine line
    let final = play grid robot actions
    printRobot final
    run grid

parseLine :: String -> (Robot, [Action])
parseLine input =
    case parse robot "stdin" input of
      Left err -> error (show err)
      Right r -> r

printRobot :: Robot -> IO ()
printRobot robot =
    let
      base =
        printf "(%d, %d, %s)"
          (robotX robot)
          (robotY robot)
          (formatOrientation $ robotOrientation robot)
    in
    if dead robot
      then putStrLn (base ++ " LOST")
      else putStrLn base

formatOrientation :: Orientation -> String
formatOrientation North = "N"
formatOrientation East = "E"
formatOrientation South = "S"
formatOrientation West = "w"
