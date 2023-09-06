module CorrecaoProva where

conv :: Int -> String
conv x | x > 359999 || x < 0 = error "bla"
       | otherwise = show hh ++ ":" ++ show mm ++ ":" ++ show ss
       where
          h = x `div` 3600
          m = (x `mod` 3600) `div` 60
          s = (x `mod` 3600) `mod` 60
          ss = if s > 59 then fst (tiraResto s 1) else s
          mm = if m > 59 then fst (tiraResto m 1) + snd (tiraResto s 1) else m + snd (tiraResto s 1)
          hh = h + snd (tiraResto m 1)

tiraResto :: Int -> Int -> (Int,Int)
tiraResto x y | x < 60 = (x,0)
              | otherwise = if (x - 60) > 59 then tiraResto (x-60) (y+1) else (x-60,y)