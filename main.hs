import Data.List
import Data.Function
import Debug.Trace
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

resultBoard :: [Int]
resultBoard = [0, 4, 3, 1, 0, 0,
               0, 2, 4, 3, 5, 1,
               0, 3, 1, 5, 4, 2,
               4, 5, 2, 6, 3, 0,
               3, 6, 5, 4, 2, 0,
               0, 0, 0, 2, 1, 4]

testBoard :: [Int]
testBoard = [0,2,3,1,0,0,0,1,2,3,5,6,0,3,1,5,4,2,4,5,6,2,3,0,3,6,5,4,2,0,0,0,0,6,1,4]

-- transformador de indice para coordenaqda
itop :: Int -> (Int, Int)
itop index = (coordX index, coordY index)
    where coordY index = index - 6 * (index `div` 6)
          coordX index = index `div` 6

-- trasformador coordenada para indice
ptoi :: (Int, Int) -> Int
ptoi (x, y) = (x*6) + y

-- pega as colunas de numberBoard ou colorBoard
getColumn :: Int -> [t] -> [t]
getColumn index board = auxGet (itop index) board
    where auxGet (x, _) board = map (\y -> board !! ptoi (x, y)) [0..5]

-- pega as linhas de numberBoard ou colorBoard
getRow :: Int -> [t] -> [t]
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

-- valores validos no indice index
getOptions :: Int -> [Int] -> [Int]
getOptions index board | index > length board  = []
                    --    | colorBoard !! index == False = []
                       | (board !! index) == 0 = [n | n <- [1..6], not $ any (n==) (getRow index board), not $ any (n==) (getColumn index board)]
                       | otherwise             = [board !! index]

-- busca a proxima celular nao preenchida do tabuleiro
nextBlank :: Int -> [Int] -> Int
nextBlank index board | index == ((length board) - 1)                            = ((length board) - 1)
                      | (board !! (index + 1) == 0) && (((colorBoard) !! (index+1)) == True) = index + 1
                      | otherwise                                                = nextBlank (index + 1) board


-- cria um novo tabuleiro substituindo o valor "value" no indice "index"
try :: Int -> [Int] -> Int -> [Int]
try index board value = take index board ++ [value] ++ drop (index + 1) board

-- testador de possiveis soluções recursivas
solve :: Int -> [Int] -> [Int] -> [Bool] -> [Int]
solve 35 board [] colors     = []
solve 35 board (x:[]) colors | isFinished (try 35 board x) colorBoard = try 35 board x
                             | otherwise = []
solve 35 board (x:_) colors  = []
solve _ board [] colors      = []
solve index board (value:values) colors | (tryNext == []) = (solve index board values colors)
                                        | otherwise       = (tryNext)
    where solveNext index board colors  = solve (nextBlank index board) board (getOptions (nextBlank index board) board) colors
          tryNext                       = solveNext index (try index board value) colors

main = do
    print $ solve 1 numberBoard (getOptions 1 numberBoard) colorBoard
    -- print $ getSequences numberBoard colorBoard 0
    -- print $ getSeqColumn numberBoard colorBoard 0
    -- print $ getSeqLine numberBoard colorBoard 1
    -- trace("board= "++ show board)
    -- print $ getRow 3 numberBoard
    -- print $ getSequences numberBoard colorBoard 0

    -- print $ isFinished resultBoard colorBoard
    -- print $ splitOn [(1, True),(2, True),(-1,False),(3, True),(4, True),(5, True)]