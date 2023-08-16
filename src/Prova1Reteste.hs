module Prova1Reteste where

type Funcionario = (String, [String])
type Dependente = String

--questao 1
dependentes :: [Funcionario] -> [Dependente]
dependentes [] = []
dependentes ((_,deps):fs) = deps ++ dependentes fs

dependentes' :: [Funcionario] -> [Dependente]
dependentes' = foldl (\sem ele -> sem ++ (snd ele)) []

dependentes'' :: [Funcionario] -> [Dependente]
dependentes'' xs = concat [y | (_,y) <- xs]

--questao2
menor :: [Int] -> Int
menor [] = error "lista vazia"
menor (x:xs)= foldr min x xs

removeMin :: [Int] -> [Int]
removeMin [] = []
removeMin l = remove (menor l) l

remove :: Int -> [Int] -> [Int]
remove _ [] = []
remove x (y:ys) = if x == y then ys else y : remove x ys

--questao 3
humanReadable :: Int -> String
humanReadable x = format horas minutos segundos
                  where
                      segundos = x `mod` 60
                      minutosTotal = x `div` 60
                      minutos =  minutosTotal `mod` 60
                      horas = minutosTotal `div` 60
                      
format h m s = twoDigit h ++ ":" ++ twoDigit m ++ ":" ++ twoDigit s
               where
                   twoDigit v = if v > 9 then show v else "0" ++ show v

--questao 4
escolheFuncoes :: [a -> Bool] -> a -> [a -> Bool]
escolheFuncoes [] _ = []
escolheFuncoes fs p = [f | f <- fs, not (f p)]

aplica :: [a -> Bool] -> a -> [Bool]
aplica [] _ = []
aplica fs p = [f p | f <- fs]