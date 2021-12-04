module Main where

data Command = Forward Integer
             | Down Integer
             | Up Integer
             | Noop
             deriving Show

data Position = Pos {horizontal::Integer, depth::Integer, aim::Integer} deriving Show

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
advancePosition pos (Forward n) = pos { horizontal = horizontal pos + n, depth = depth pos + (aim pos * n) }
advancePosition pos (Down n) = pos { aim = aim pos + n }
advancePosition pos (Up n) = pos { aim = aim pos - n }
advancePosition pos _ = pos

calculatePosition :: Position -> [Command] -> Position
calculatePosition = foldl advancePosition

main :: IO ()
main = do
    content <- readFile "input.txt"
    let linesOfContent = lines content
        commands = getCommands linesOfContent
        startPosition = Pos {horizontal = 0, depth = 0, aim = 0}
        finalPosition = calculatePosition startPosition commands
    print finalPosition
    print (horizontal finalPosition * depth finalPosition)
