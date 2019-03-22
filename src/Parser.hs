module Parser (grid, robot) where

import Text.Parsec
import Game

type Parser a = Parsec String () a

grid :: Parser Grid
grid =
    do x <- number
       space
       y <- number
       return $ Grid x y

robot :: Parser (Robot, [Action])
robot =
    do char '('
       x <- number
       char ','
       skipMany1 space
       y <- number
       char ','
       skipMany1 space
       o <- orientation
       char ')'
       skipMany1 space
       as <- actions
       skipMany space
       return $ (Robot x y o False, as)


number :: Parser Int
number =
    read <$> many1 digit

orientation :: Parser Orientation
orientation =
    (char 'N' >> return North)
     <|> (char 'E' >> return East)
     <|> (char 'S' >> return South)
     <|> (char 'W' >> return West)

actions :: Parser [Action]
actions =
  many action

action :: Parser Action
action =
    (char 'L' >> return L)
     <|> (char 'R' >> return R)
     <|> (char 'F' >> return F)
    
