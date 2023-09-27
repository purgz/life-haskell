import Control.Concurrent

--grid is a list of cells with x,y coords
type Cell = (Int, Int)
type Grid = [Cell]

isLive, isDead :: Cell -> Grid -> Bool
isLive cell grid = cell `elem` grid
isDead cell grid = not (isLive cell grid)

neighbours :: Cell -> [Cell]
neighbours (x,y) = [(x + i, y + j) |
    i <- [-1..1],
    j <- [-1..1],
    not (i == 0 && j == 0)]

liveNeighbours :: Cell -> Grid -> [Cell]
liveNeighbours cell grid = [c' | c' <- neighbours cell, isLive c' grid]

step :: Grid -> Grid

step [] = []

step grid = [(x,y) |
  x <- [minX - 1..maxX + 1],
  y <- [minY - 1..maxY + 1],
  ((isDead (x,y) grid && length(liveNeighbours (x,y) grid) == 3) ||
  (isLive (x,y) grid && length(liveNeighbours (x,y) grid) `elem` [2,3]))]
  where 
    minX = minimum[x | (x,y) <- grid]
    maxX = maximum[x | (x,y) <- grid]
    minY = minimum[y | (x,y) <- grid]
    maxY = maximum[y | (x,y) <- grid]



p :: Grid
p = [(1,2), (2,2), (2,3), (4,1), (4,3)]

glider :: Grid
glider = [(1,3), (2,1), (2,3), (3,2), (3,3)]

--visualizing in terminal
terminalWidth = 70
terminalHeight = 22

cls :: IO()
cls = putStr "\ESC[2J]"

goto :: Cell -> IO()
goto (x,y) = putStr ("\ESC[" ++ show(terminalHeight-y) ++ ";" ++ show(x+1) ++ "H")

printCell :: Cell -> IO()
printCell (x,y) | x >= 0 && x < terminalWidth
  && y >= 0 && y < terminalHeight = do 
    goto(x,y)
    putChar '0'
  | otherwise = return ()

terminalRender :: Grid -> IO()
terminalRender grid = do
  cls
  sequence [printCell c | c <- grid]
  goto (0, terminalHeight)

delayTenthSec :: Int -> IO()
delayTenthSec n = threadDelay (n * 10^5)

life :: Grid -> IO()
life seed = f 0 seed
  where f n g = do
                  terminalRender g
                  putStrLn(show n)
                  delayTenthSec 1
                  f (n+1) (step g)

main :: IO()
main = life glider