-- mapa do tabuleiro
numberBoard :: [Int]
numberBoard = [0, 0, 0, 1, 0, 0,
               0, 0, 0, 0, 5, 0,
               0, 0, 1, 0, 0, 0,
               4, 0, 0, 0, 0, 0,
               0, 6, 5, 0, 0, 0,
               0, 0, 0, 0, 1, 4]

-- cores do tabuleiro
colorBoard :: [Bool]
colorBoard = [False, True,  True,  False, False, False,
              False, True,  True,  True,  True,  True,
              False, True,  True,  True,  True,  True,
              True,  True,  True,  True,  True,  False,
              True,  True,  True,  True,  True,  False,
              False, False, False, True,  True,  False]

-- indice para coordenaqda
itop :: Int -> (Int, Int)
itop index = (coordX i, coordY i)
    where coordX i = i - 9 * (i `div` 9)
          coordY i = i `div` 9

-- coordenada para indice
ptoi :: (Int, Int) -> Int
ptoy (x, y) = x + y * 9

-- pega as colunas de numberBoard ou colorBoard
getColumn :: Int -> [t] -> [t]
getColumn index board = auxGet (itop index) board
    where auxGet (x, _) board = map (\y -> board !! ptoi (x, y)) [0..8]

-- pega as linhas de numberBoard ou colorBoard
getRow :: Int -> [Int] -> [Int]
getRow index board = auxGet (itop index) board
    where auxGet (_, y) board = map (\x -> s !! ptoi (x, y)) [0..8]

getOptions :: Int -> [Int] -> [Int]

-- tamanho de array
len :: [t] -> Int
len [] = 0
len (_ : b) = 1 + len b

-- busca a proxima celular nao preenchida do tabuleiro
nextBlank :: Int -> [Int] -> Int
nextBlank index board | p = ((len board) - 1)     = ((len board) - 1)
                      | board !! (index + 1) == 0 = index + 1
                      | otherwise                 = nextBlank (index + 1) board

-- cria um novo tabuleiro substituindo o valor "value" no indice "index"
try :: Int -> [Int] -> Int -> [Int]
try index board value = take index board ++ [value] ++ drop (index + 1) board

-- testador de possiveis soluções recursivas
solve :: Int -> [Int] -> [Int] -> [Int]
solve index board (value:values) | tryNext == [] = solve index board values
                                 | otherwise     = tryNext
    where solveNext index board = solve (nextBlank index board) board (getOptions (nextBlank index board) board)
          tryNext               = solveNext index (try index board value)