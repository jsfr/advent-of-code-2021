module Main where

data Command = Forward Integer
             | Down Integer
             | Up Integer
             | Noop
             deriving Show

type Position = (Integer, Integer)

toCommand :: String -> Integer -> Command
toCommand "up" n = Up n
toCommand "down" n = Down n
toCommand "forward" n = Forward n
toCommand _ _ = Noop

getCommand :: String -> Command
getCommand str = let (command:v:_) = words str
                     value = read v :: Integer
                 in toCommand command value

getCommands :: [String] -> [Command]
getCommands = map getCommand

advancePosition :: Position -> Command -> Position
advancePosition (h, v) (Forward n) = (h + n, v)
advancePosition (h, v) (Down n) = (h, v + n)
advancePosition (h, v) (Up n) = (h, v - n)
advancePosition pos _ = pos

calculatePosition :: Position -> [Command] -> Position
calculatePosition = foldl advancePosition

main :: IO ()
main = do
    content <- readFile "input.txt"
    let linesOfContent = lines content
        commands = getCommands linesOfContent
        startPosition = (0, 0) :: Position
        finalPosition = calculatePosition startPosition commands
    print finalPosition
    print (uncurry (*) finalPosition)
