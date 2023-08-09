module Arvores (
    Arvore (..),
    existe,
    serializar,
    contar 
) 
where


data Arvore a = Folha a | No (Arvore a) a (Arvore a) deriving Show

existe :: Eq a => a -> Arvore a -> Bool
existe x (Folha y) = x == y
existe x (No esq y dir) = x == y || existe x esq || existe x dir

serializar :: Arvore a -> [a]
serializar (Folha x) = [x]
serializar (No esq x dir) = serializar esq ++ [x] ++ serializar dir

contar :: Arvore a -> Int
contar (Folha _) = 1
contar (No esq _ dir) = contar esq + contar dir + 1

instance (Eq a) => Eq (Arvore a) where
    (==) :: Arvore a -> Arvore a -> Bool
    (No x1 y1 z1) == (No x2 y2 z2) = y1 == y2 && x1 == x2 && z1 == z2
    (Folha x) == (Folha y) = x == y
    _ == _ = False

instance (Ord a) => Ord (Arvore a) where
    (<) :: Arvore a -> Arvore a -> Bool
    Folha _ < (No _ _ _) = True
    (No esq1 _ dir1) < (No esq2 _ dir2) = contar esq1 + contar dir1 < contar esq2 + contar dir2
    _ < _ = False

    (>) :: Arvore a -> Arvore a -> Bool
    Folha _ > (No _ _ _) = False
    (No esq1 _ dir1) > (No esq2 _ dir2) = contar esq1 + contar dir1 > contar esq2 + contar dir2
    _ > _ = False
    
    (<=) :: Arvore a -> Arvore a -> Bool
    Folha _ <= (No _ _ _) = True
    (No esq1 _ dir1) <= (No esq2 _ dir2) = contar esq1 + contar dir1 <= contar esq2 + contar dir2
    _ <= _ = False

    (>=) :: Arvore a -> Arvore a -> Bool
    Folha _ >= (No _ _ _) = True
    (No esq1 _ dir1) >= (No esq2 _ dir2) = contar esq1 + contar dir1 >= contar esq2 + contar dir2
    _ >= _ = False