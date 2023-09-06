module Life where

import Data.List

cls :: IO ()
cls = putStr "\ESC[2J"

type Pos = (Int, Int)

writeat :: Pos -> String -> IO ()
writeat p xs = do goto p
                  putStr xs

goto :: Pos -> IO ()
goto (x, y) = putStr ("\ESC[" ++ show y ++ ";" ++ show x ++ "H")

width :: Int
width = 10

height :: Int
height = 10

type Board = [Pos]

glider :: Board
glider = [(4,2), (2,3), (4,3), (3,4), (4,4)]

showcells :: Board -> IO ()
showcells b = sequence_ [writeat p "O" | p <- b]

isAlive :: Board -> Pos -> Bool
isAlive b p = p `elem` b

isEmpty :: Board -> Pos -> Bool
isEmpty b p = not $ isAlive b p

neighbors :: Pos -> [Pos]
neighbors (x,y) = map wrap [(x - 1, y - 1), (x, y - 1),
                            (x + 1, y - 1), (x - 1, y),
                            (x + 1, y), (x - 1, y + 1),
                            (x, y + 1), (x + 1, y + 1)]

wrap :: Pos -> Pos
wrap (x,y) = (((x - 1) `mod` width) + 1, ((y - 1) `mod` height) + 1)

liveneighbors :: Board -> Pos -> Int
liveneighbors b = length . filter (isAlive b) . neighbors

survivors :: Board -> [Pos]
survivors b = [p | p <- b, liveneighbors b p `elem` [2, 3]]

births :: Board -> [Pos]
births b = [p | p <- nub (concatMap neighbors b), isEmpty b p, liveneighbors b p == 3]

nextgen :: Board -> Board
nextgen b = survivors b ++ births b

life :: Board -> IO ()
life b = do cls
            showcells b
            wait 500000
            life $ nextgen b

wait :: Int -> IO ()
wait n = sequence_ [return () | _ <- [1 .. n]]

