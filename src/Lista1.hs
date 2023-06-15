module Lista1 (
    iguais,
    maioresQueMedia,
    potencia_2,
    potencia_4,
    meuXor,
    xMenor,
    xMaior,
    somaExclusiva,
    somaInclusiva,
    multiplos,
    multiplicarSoma,
    mod2,
    seq6,
    fat,
    arranjo,
    maiorLista,
    dic_10,
    converter,
    consultar,
    delPosicao,
    inserirPosicaoX
    ) where

iguais :: Eq a => a -> a -> a -> Int
iguais a b c
    | a == b && a == c = 3
    | a == b || a == c || b == c = 2
    | otherwise = 0

media3 :: Fractional a => a -> a -> a -> a
media3 a b c = (a + b + c) / 3

maior :: (Ord a, Num b) => a -> a -> b
maior x y = if x > y then 1 else 0

maioresQueMedia :: (Num a1, Ord a2, Fractional a2) => a2 -> a2 -> a2 -> a1
maioresQueMedia a b c = a `maior` m + maior b m + maior c m
                        where
                            m = media3 a b c


potencia_2 :: Num a => a -> a
potencia_2 x = x * x

potencia_4 :: Num a => a -> a
potencia_4 x = p * p
               where
                p = potencia_2 x

meuXor :: Bool -> Bool -> Bool
meuXor True True = False
meuXor False False = False
meuXor _ _ = True

xMenor :: Floating a => a -> a -> a -> a 
xMenor a b c = ((-b) - raizDelta a b c)  / (2 * a)

xMaior :: Floating a => a -> a -> a -> a 
xMaior a b c = ((-b) + raizDelta a b c)  / (2 * a)

raizDelta :: Floating a => a -> a -> a -> a
raizDelta a b c = sqrt $ b * b - 4 * a * c

somaInclusiva :: Int -> Int -> Int
somaInclusiva x y
    | x < y = sum [x..y]
    | otherwise = sum [y .. x]


somaExclusiva :: Int -> Int -> Int
somaExclusiva x y
    | x == y = 0
    | otherwise = somaInclusiva x y - x - y

multiplos :: Integral a => a -> a -> a -> [a]
multiplos x y z = [n | n <- limite, mod n z == 0]
    where
        limite = if x > y then [y..x] else [x..y]

multiplicarSoma :: Num a => Int -> a -> a
multiplicarSoma x y = sum $ x `replicate` y

mod2 :: (Ord t, Num t) => t -> t -> t
mod2 x y = if x >= y then mod2 (x - y) y else x


seq6 :: (Eq t, Floating a, Num t) => t -> a
seq6 n = if n == 1 then sqrt 6 else sqrt (6 + seq6 (n-1))

fat :: Int -> Int
fat n = product [1 .. n]

arranjo :: Int -> Int -> Int
arranjo n p = fat n `div` fat (n - p)

maiorLista :: Ord a => [a] -> (a, Int)
maiorLista [] = error "lista vazia"
maiorLista l = (m, pos m l)
    where
        m = maximum l

pos :: (Eq t, Num a) => t -> [t] -> a
pos _ [] = error "lista vazia"
pos e (x:xs)
    | x == e = 0
    | otherwise = 1 + pos e xs

dic_10 :: [(Int, String)]
dic_10 = [(0,"zero"), (1,"um"), (2, "dois"), (3, "três"), (4, "quatro"), (5, "cinco")]

converter :: [Int] -> [String]
converter [] = []
converter (a:as) = consultar a dic_10 : converter as -- : é o operador cons

consultar :: Eq t => t -> [(t, String)] -> String
consultar _ [] = ""
consultar ch ((k,v):xs) = if k == ch then v else consultar ch xs

delPosicao :: Int -> [Int] -> [Int]
delPosicao p l = take p l ++ drop (p + 1) l

inserirPosicaoX :: [Int] -> Int -> Int -> [Int]
inserirPosicaoX l p v = take p l ++ [v] ++ drop p l