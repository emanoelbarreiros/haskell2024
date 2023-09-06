module Nim2 where

import Data.Char

type Board = [Int]

initial :: Board
initial = [5,4,3,2,1]

putRow :: Int -> Int -> IO ()
putRow row num = do putStr (show row)
                    putStr ": "
                    putStrLn (concat (take num (repeat "* ")))

putBoard :: Board -> IO ()
putBoard l = mapM_ (uncurry putRow) (zip [1 ..] l)

finished :: Board -> Bool
finished b = not (any (/= 0) b)-- or (map (/= 0) b)

move :: Board -> Int -> Int -> Board
move b r n = map (\(p,qtd) -> if p == r then qtd - n else qtd) (zip [1..] b)

valid :: Board -> Int -> Int -> Bool
valid b r n = r > 0 && r <= length b && b !! (r - 1) >= n

next :: Int -> Int
next 1 = 2
next 2 = 1

isDigit :: Char -> Bool
isDigit c = c `elem` ['0'..'9']

getDigit :: String -> IO Int
getDigit msg = do putStr msg
                  c <- getChar
                  if Nim2.isDigit c then
                    do putStr "\n"
                       return (digitToInt c)
                  else
                    Nim2.getDigit "ERRO. Informe um caractere válido: "
                       


play :: Board -> Int -> IO ()
play b p = 
    do putChar '\n'
       putBoard b
       if finished b then
          do putChar '\n'
             putStr "Jogador "
             putStr (show (next p))
             putStr " venceu!"
       else
          do putChar '\n'
             putStr "Jogador "
             putStrLn (show p)
             r <- Nim2.getDigit "Informe uma linha: "
             n <- Nim2.getDigit "Informe a quantidade de estrelas: "
             if valid b r n then
                play (move b r n) (next p)
             else
                do putStr "\n"
                   putStrLn "ERRO: jogada inválida."
                   play b p




