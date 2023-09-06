module Forca where

import System.IO

forca :: IO ()
forca = do hSetBuffering stdout NoBuffering
           putStrLn "Escreva uma palavra: "
           palavra <- obterLinhaSecreta
           putStrLn "Tente adivinhar: "
           jogar palavra

obterLinhaSecreta :: IO String
obterLinhaSecreta = do x <- obterChar
                       if x == '\n' then
                           do putChar x
                              return []
                       else
                           do putChar '-'
                              xs <- obterLinhaSecreta
                              return (x:xs)


obterChar:: IO Char
obterChar = do hSetEcho stdin False
               x <- getChar
               hSetEcho stdin True
               return x


jogar :: String -> IO ()
jogar palavra = do putStr "? "
                   tentativa <- getLine

                   if tentativa == palavra then
                       putStrLn "Muito bem!"
                   else do putStrLn (encontrar palavra tentativa)
                           jogar palavra

encontrar :: String -> String -> String
encontrar xs ys = [if x `elem` ys then x else '-' | x <- xs]