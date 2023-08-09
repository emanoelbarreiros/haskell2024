module Natural (
    somar,
    flatten,
    NestedList (..)
)
where

data Nat = Zero | Suc Nat

somar :: Nat -> Nat -> Nat
somar Zero n = n
somar (Suc m) n = Suc $ somar m n

data NestedList a = Elem a | List [NestedList a]

flatten :: NestedList a -> [a]
flatten (Elem x) = [x]
flatten (List []) = []
flatten (List (x:xs)) = flatten x ++ flatten (List xs)

--flatten (Elem 5) = [5]
--flatten (List [Elem 1, List [Elem 2, List [Elem 3, Elem 4], Elem 5]]) = [1,2,3,4,5]