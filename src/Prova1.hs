module Prova1 (
    funcionarios,
    prefixos33,
    primo,
    primos,
    tail1,
    tail2,
    tail3,
    novoMap,
    meuSplit,
    prefixos
) where

--Questao 1
funcionarios :: [(String, [String])] -> [String]
funcionarios lista = [nomeFunc | (nomeFunc, dependentes) <- lista, length dependentes >= 2]

--Questao 2
prefixos33 :: [a] -> [[a]]
prefixos33 []     = []
prefixos33 xs = foldr (\x y -> map (x:) ([]:y)) [] xs

--Questao 3
primo :: Integral a => a -> Bool
primo x = not $ or [x `mod` y == 0 | y <- [2..x - 1]]

primos :: Integral a => a -> a -> [a]
primos x y = [z | z <- [x .. y], primo z]

-- questão 4
tail1 :: [a] -> [a]
tail1 l = if null l then [] else tail l

tail2 :: [a] -> [a]
tail2 l 
    | null l = []
    | otherwise = tail l

tail3 :: [a] -> [a]
tail3 [] = []
tail3 (_:xs) = xs


-- questao 5
novoMap :: [b -> b] -> [b] -> [b]
novoMap [] xs = xs
novoMap _ [] = []
--novoMap (f:fs) xs = novoMap fs (map f xs)
novoMap fs xs = foldl (flip map) xs fs


meuSplit :: Eq a => [a] -> a -> [[a]]
meuSplit s c = aux s c [] []

aux :: Eq a => [a] -> a -> [a] -> [[a]] -> [[a]]
aux [] _ parc res = res ++ [parc]
aux (s:ss) c parc res 
    | s /= c = aux ss c (parc ++ [s]) res
    | otherwise = aux ss c [] (res ++ [parc])

prefixos :: [a] -> [[a]]
prefixos [] = []
prefixos (x:xs) = let pfxs = prefixos xs
                  in [x] : map (x:) pfxs