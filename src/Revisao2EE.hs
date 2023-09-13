module Revisao2EE where

import System.IO

putStr2 :: String -> IO ()
putStr2 s = sequence_ [putChar x | x <- s]

obterLinha :: IO String
obterLinha = do x <- obterChar
                if x == '\n' then
                    do putChar x
                       return []
                else
                    if x == '\DEL' then
                        do return ['\DEL']
                    else 
                        do putChar x
                           xs <- obterLinha
                           if xs == ['\DEL'] then
                              do putChar '\b' 
                                 putChar ' '
                                 putChar '\b'
                                 obterLinha
                           else
                              do return (x:xs)

obterChar:: IO Char
obterChar = do hSetEcho stdin False
               x <- getChar
               hSetEcho stdin True
               return x

data Expr = Val Int | Add Expr Expr

folde :: (Int -> a) -> (a -> a -> a) -> Expr -> a
folde f g (Add e1 e2) = g (folde f g e1) (folde f g e2)
folde f _ (Val x) = f x

--Add (Val 1) (Val 2)
eval :: Expr -> Int
eval = folde id (+) 

size :: Expr -> Int
size = folde (const 1) (+)

data Arvore a = Folha a | No (Arvore a) (Arvore a) deriving Show

dividirLista :: [a] -> ([a], [a])
dividirLista l = (take (div (length l) 2) l, drop (div (length l) 2) l)

balancear :: [a] -> Arvore a
balancear [e] = Folha e
balancear l = No (balancear esq) (balancear dir)
              where
                 (esq, dir) = dividirLista l



data Arvore2 a = Folha2 a | No2 (Arvore2 a) a (Arvore2 a) deriving Show

existe :: Ord a => a -> Arvore2 a -> Bool
existe y (Folha2 x) = compare x y == EQ
existe y (No2 esq v dir) = case compare y v of
                                LT -> existe y esq
                                EQ -> True
                                GT -> existe y dir