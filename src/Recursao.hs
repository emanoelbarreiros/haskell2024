module Recursao (inserir, ordenar) where


--inserir elemento em uma lista ordenada
inserir :: Ord t => t -> [t] -> [t]
inserir x [] = [x]
inserir x (y:ys)
    | x <= y = x : y : ys
    | otherwise = y : inserir x ys

--ordenar lista
ordenar :: Ord a => [a] -> [a]
ordenar [] = []
ordenar (x:xs) = inserir x (ordenar xs)