module Lib
    ( someFunc, meuLast) where

someFunc :: IO ()
someFunc = putStrLn "someFunc"


meuLast :: [a] -> a
meuLast [] = error "lista vazia nao permitida"
meuLast [x] = x
meuLast l = head (reverse l)