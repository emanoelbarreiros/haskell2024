module Bee1241 where

import Data.List


obterEntrada :: IO (String, String)
obterEntrada = do entrada <- getLine
                  let parte1 = takeWhile (/= ' ') entrada
                  let parte2 = tail $ dropWhile (/= ' ') entrada
                  return (parte1, parte2)


main :: IO ()
main = do casos <- getLine
          let qtd = read casos :: Int
          entradas <- sequence $ replicate qtd obterEntrada
          let resultadosBool = map (uncurry $ flip isSuffixOf) entradas
          let resultadosString = map (\b -> if b then "encaixa" else "nao encaixa") resultadosBool
          mapM_ putStrLn resultadosString
