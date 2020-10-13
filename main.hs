import Data.List
import Data.Function
import Debug.Trace


-- Mapa do tabuleiro
-- Aqui, todo número != 0 representa
-- um número presente no tabuleiro inicial
numberBoard :: [Int]
numberBoard = [3, 0, 0, 4, 0, 0,
               0, 0, 0, 0, 0, 0,
               0, 0, 0, 0, 0, 2,
               4, 0, 0, 0, 0, 3,
               0, 0, 0, 0, 0, 5,
               0, 0, 3, 0, 1, 0]


-- Cores do tabuleiro:
-- Nesta lista, valores False representam
-- Células de cor Preta, ou seja, cujos valores não podem
-- ser alterados. Valores True são células Brancas,
-- cujo valor pode ser alterado pelo algoritmo
colorBoard :: [Bool]
colorBoard = [False, False, True,  True,  True,  False,
              False, True,  True,  True,  True,  False,
              True,  True,  False, False, True,  True,
              True,  True,  False, False, True,  True,
              False, True,  True,  True,  True,  False,
              False, True,  True,  True,  False, False]


-- Transformador de índice -> coordenaqda
-- Esta função recebe um int que representa um
-- índice na lista numberBoard e o converte para
-- coordenadas bidimencionais (i, j), para ser
-- interpretado como elemento de uma matriz 6x6
itop :: Int -> (Int, Int)
itop index = (coordX index, coordY index)
    where coordY index = index - 6 * (index `div` 6)
          coordX index = index `div` 6


-- Trasformador coordenada -> índice
-- Essa função faz o inverso da função anterior
-- Recebe uma tupla (x, y) que representa um
-- elemento de uma matriz 6x6 e converte para
-- um índice de uma Lista de 36 elementos.
ptoi :: (Int, Int) -> Int
ptoi (x, y) = (x*6) + y


-- getColumn recebe um índice e o tabuleiro, e
-- utiliza a função auxiliar auxGet, que é
-- uma expressão lambda utilizada  para trazer,
-- a partir do índice fornecido, todos os elementos
-- da coluna a qual o elemento pertence
getColumn :: Int -> [t] -> [t]
getColumn index board = auxGet (itop index) board
    where auxGet (x, _) board = map (\y -> board !! ptoi (x, y)) [0..5]


-- getRow funciona da mesma maneira que a função
-- getColumn explicada anteriormente: recebe um índice
-- e utiliza a função auxiliar auxGet, que converte esse
-- indice em coordeanadas do tabuleiro, e retorna os elementos
-- da linha a qual o indice pertence.
getRow :: Int -> [t] -> [t]
getRow index board = auxGet (itop index) board
    where auxGet (_, y) board = map (\x -> board !! ptoi (x, y)) [0..5]


-- A função remove recebe duas listas e
-- remove da segunda lista os elementos que
-- estão na primeira. Ela funciona através de
-- uma chamada para a função actRemove
remove :: [Int] -> [Int] -> [Int]
remove [] _      = []
remove values [] = values
remove xs (y:ys) = remove (actRemove y xs) ys


-- Esta função recebe um elemento e uma Lista,
-- e remove todas as ocorrencias desse elemento
actRemove :: Int -> [Int] -> [Int]
actRemove _ [] = []
actRemove a (x:xs) | x == a    = actRemove a xs
                   | otherwise = x : actRemove a xs

-- pega todas as sequencias existentes no tabuleiro
getSequences :: [Int] -> [Bool] -> Int -> [[Int]]
getSequences board color 6 = []
getSequences board color index = (getSeqColumn board color index) ++ (getSeqLine board color index) ++ getSequences board color (index + 1)

--  divide as sequencias recebidas por uma lista quando o booleano de sua tupla for False
splitOn :: [(Int,Bool)] -> [[Int]]
splitOn [] = []
splitOn l  = filter (\l -> length l /= 0) [[i | (i,j) <- takeWhile (snd) l]] ++ splitOn (dropWhile (not . snd) (dropWhile (snd) l))

-- pega as sequencias presentes em uma coluna
getSeqColumn :: [Int] -> [Bool] -> Int -> [[Int]]
getSeqColumn board color column = splitOn [(num,bol) | (num,bol) <- zip (getColumn columnIndex board) (getColumn columnIndex color)]
    where columnIndex = column * 6

-- pega as sequencias presentes em uma linha
getSeqLine :: [Int] -> [Bool] -> Int -> [[Int]]
getSeqLine board color line = splitOn [(num,bol) | (num,bol) <- zip (getRow line board) (getRow line color)]


isFinished :: [Int] -> [Bool] -> Bool
isFinished board color = all (True==) (map isSequence (getSequences board color 0))

-- verifica se as sequencias presentes na lista recebida são validas
isSequence :: [Int] -> Bool
isSequence list = (((sort list) !! (length list - 1)) - ((sort list) !! 0)) == length list - 1

-- A função getOptions recebe um índice
-- e o tabuleiro, e retorna, para esse índice
-- todas as opções válidas de valores que podem ser
-- inseridos nela.
getOptions :: Int -> [Int] -> [Int]
getOptions index board | index > length board  = []
                    --    | colorBoard !! index == False = []
                       | (board !! index) == 0 = [n | n <- [1..6], not $ any (n==) (getRow index board), not $ any (n==) (getColumn index board)]
                       | otherwise             = [board !! index]


-- Esta função busca a proxima célula
-- do tabuleiro que ainda não foi preenchida
-- com nenhum valor
nextBlank :: Int -> [Int] -> Int
nextBlank index board | index == ((length board) - 1) && (colorBoard !! index) == True       = ((length board) - 1)
                      | index == ((length board) - 1) && (colorBoard !! index) == False      = length board
                      | (board !! (index + 1) == 0) && (((colorBoard) !! (index+1)) == True) = index + 1
                      | otherwise                                                = nextBlank (index + 1) board


-- Essa função recebe um índice e verifica
-- se o mesmo representa uma célula válida,
-- ou seja, se é um quadrado Branco, que pode
-- ser alterado.
isValid :: Int -> [Bool] -> Int
isValid index board | (board !! index) == False = isValid (index + 1) board
                    | otherwise = index

-- A função Try recebe um índice i, o tabuleiro
-- e um valor x, e insere x na posição i do tabuleiro
try :: Int -> [Int] -> Int -> [Int]
try index board value = take index board ++ [value] ++ drop (index + 1) board


-- A função solve realiza o backtracking sobre o tabuleiro
-- procurando por soluções. Ela possui duas funções auxiliares
-- solveNext e tryNext, que buscam a próxima célula válida do
-- tabuleiro e realizam testes com os possíveis valores
solve :: Int -> [Int] -> [Int] -> [Bool] -> [Int]
solve 36 board _ colors | isFinished board colors = board
                        | otherwise = []
solve 35 board [] colors     = []
solve 35 board (x:b) colors | isFinished (try 35 board x) colors = try 35 board x
                            | otherwise = solve 35 board b colors
solve _ board [] colors      = []
solve index board (value:values) colors | (tryNext == []) = (solve index board values colors)
                                        | otherwise       = (tryNext)
    where solveNext index board colors  = solve (nextBlank index board) board (getOptions (nextBlank index board) board) colors
          tryNext                       = solveNext index (try index board value) colors

joinWith :: a -> [a] -> [a]
joinWith _ (x:[])  = [x]
joinWith c (x:xs)  = x : c : joinWith c xs

pPrint [] = []
pPrint s  = spaceOut s ++ pPrint (drop 6 s)
  where showS s    = concatMap show s
        space      = ' '
        newline    = "\n"
        spaceOut s = joinWith space (take 6 (showS s) ++ newline)

fullSolve = putStrLn $ pPrint $ solve (isValid 0 colorBoard) numberBoard (getOptions (isValid 0 colorBoard) numberBoard) colorBoard



-- para executar, rodar ghci e o seguinte comando
-- putStrln $ pPrint $ solve (isValid 0 colorBoard) numberBoard (getOptions (isValid 0 colorBoard) numberBoard) colorBoard
