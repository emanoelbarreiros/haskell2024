module Nim where

import Data.Char


--O jogo de Nim é um interessante jogo de estratégia, 
-- onde dois jogadores competem para ser o último jogador a retirar estrelas do tabuleiro.
--Em turnos, cada jogador escolher uma linha e a quantidade de estrelas que deseja remover.
--Vence o jogador que deixar o tabuleiro vazio, removendo a(s) última(s) estrela(s).
-- 1: * * * * *
-- 2: * * * *
-- 3: * * *
-- 4: * *
-- 5: *

--Iniciamos definindo o tabuleiro como um novo tipo, 
-- onde cada linha é representada por um inteiro que 
-- representa quantas estrelas existem naquela linha.

type Board = [Int]

--Precisamos também definir o estado inicial do tabuleiro.
--Seguindo então o tipo que definimos, podemos ter:

initial :: Board
initial = [5,4,3,2,1]

--Podemos tentar imprimir o tabuleiro e ver como ele fica na tela.
--Começamos com uma função que imprime uma linha, depois passamos para
-- uma função que usa esta função imprimir todo o tabuleiro.

putRow :: Int -> Int -> IO ()
putRow row num = do putStr (show row)
                    putStr ": "
                    putStrLn (concat (replicate num "* "))

putBoard :: Board -> IO ()
putBoard l = mapM_ (uncurry putRow) (zip [1..] l)


--Um bom próximo passo é criar a função que calcula se o jogo terminou.
--Precisamos que ela verifique o tabuleiro e cheque se todas as linhas foram zeradas.

finished :: Board -> Bool
finished = all (== 0)


--Temos muitas possibilidades para cotinuar, mas uma possibilidade
-- é fazer a função que realiza um movimento no tabuleiro.
--Esta função deve remover uma quantidade de estrelas de uma determinada
-- linha de um tabuleiro.

move :: Board -> Int -> Int -> Board
move board row num = [update r n | (r,n) <- zip [1..] board]
                     where
                        update r n = if r == row then n - num else n

--Uma jogada, entretanto só pode ser realizada se ela é válida.
--Uma jogada é válida quando o jogador tenta remover uma quantidade
-- de estrelas menor ou igual ao número de estrelas de uma determinada
-- linha.

valid :: Board -> Int -> Int -> Bool
valid board row num = row > 0 && length board > row - 1 && board !! (row - 1) >= num


--O loop principal vai ser responsável por determinar o vencedor
-- e alternar os jogadores enquanto não haja vencedor. Ele começa,
-- verificando se o jogo terminou, em caso positivo, o jogador 
-- anterior é o vencedor. Caso contrário, o jogo continua, alternando o jogador 
-- e pedindo uma jogada.

play :: Board -> Int -> IO ()
play board player = 
    do putChar '\n'
       putBoard board
       if finished board then 
          do putChar '\n'
             putStr "Jogador "
             putStr $ show $ next player
             putStrLn " venceu!"
       else
          do putChar '\n'
             putStr "Player "
             print player
             row <- getDigit "Entre o número de uma linha: "
             num <- getDigit "Quantidade de estrelas para remover: "
             if valid board row num then
                play (move board row num) (next player)
             else
                do putChar '\n'
                   putStrLn "ERRO: jogada inválida."
                   play board player


--Precisamos implementar algumas funções que estão faltando aqui.
--A primeira é a função que alterna entre dois jogadores.
--Uma implementação muito simples é:

next :: Int -> Int
next 1 = 2
next 2 = 1


--Outra função que precisamos é a função que exibe uma mensagem 
-- para o jogador e captura um char do teclado. Ela também deve 
-- ser capaz de entender se o jogador informou um valor válido,
-- que deve ser um dígito numérico

getDigit :: String -> IO Int
getDigit prompt = do putStr prompt
                     x <- getChar
                     putChar '\n'
                     if isDigit x then
                        return (digitToInt x)
                     else
                        do putStrLn "ERRO: Dígito inválido"
                           getDigit prompt



--Precisamos agora apenas de uma função que inicia a partida.

nim :: IO ()
nim = play initial 1