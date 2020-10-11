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

-- transformador de indice para coordenaqda
itop :: Int -> (Int, Int)
itop index = (coordX index, coordY index)
    where coordX index = index - 6 * (index `div` 6)
          coordY index = index `div` 6

-- trasformador coordenada para indice
ptoi :: (Int, Int) -> Int
ptoi (x, y) = x + y * 6

-- pega as colunas de numberBoard ou colorBoard
getColumn :: Int -> [t] -> [t]
getColumn index board = auxGet (itop index) board
    where auxGet (x, _) board = map (\y -> board !! ptoi (x, y)) [0..5]

-- pega as linhas de numberBoard ou colorBoard
getRow :: Int -> [Int] -> [Int]
getRow index board = auxGet (itop index) board
    where auxGet (_, y) board = map (\x -> board !! ptoi (x, y)) [0..5]

-- remove elementos de uma uma lista que estao presentes na segunda lista
remove :: [Int] -> [Int] -> [Int]
remove [] _      = []
remove values [] = values
remove xs (y:ys) = remove (actRemove y xs) ys

-- remove as ocorrencias de a em uma lista
actRemove :: Int -> [Int] -> [Int]
actRemove _ [] = []
actRemove a (x:xs) | x == a    = actRemove a xs
                   | otherwise = x : actRemove a xs

-- valores validos no indice index
getOptions :: Int -> [Int] -> [Int]
getOptions index board | index > length board  = []
                       | (board !! index) == 0 = [n | n <- [1..6], not $ any (n==) (getRow index board), not $ any (n==) (getColumn index board)]
                       | otherwise             = [board !! index]

-- busca a proxima celular nao preenchida do tabuleiro
nextBlank :: Int -> [Int] -> Int
nextBlank index board | index == ((length board) - 1)     = ((length board) - 1)
                      | board !! (index + 1) == 0         = index + 1
                      | otherwise                         = nextBlank (index + 1) board

-- cria um novo tabuleiro substituindo o valor "value" no indice "index"
try :: Int -> [Int] -> Int -> [Int]
try index board value = take index board ++ [value] ++ drop (index + 1) board

-- testador de possiveis soluções recursivas
solve :: Int -> [Int] -> [Int] -> [Int]
solve index board (value:values) | tryNext == [] = solve index board values
                                 | otherwise     = tryNext
    where solveNext index board = solve (nextBlank index board) board (getOptions (nextBlank index board) board)
          tryNext               = solveNext index (try index board value)

main = do
    print $ getOptions (ptoi (1,5)) numberBoard
