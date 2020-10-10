import Data.Array
import Data.List

-- se não foi preenchida então o bloco da matriz é representado por -1
-- na hora de printar a matriz resolvida da pra trocar de Int pra String e quando for -1 so deixa um " "
matrix = array ((0,0),(5,5)) [((0,0),-1), ((0,1),-1), ((0,2),-1), ((0,3),1), ((0,4),-1), ((0,5),-1),
                              ((1,0),-1), ((1,1),-1), ((1,2),-1), ((1,3),-1), ((1,4),5), ((1,5),-1),
                              ((2,0),-1), ((2,1),-1), ((2,2),1), ((2,3),-1), ((2,4),-1), ((2,5),-1),
                              ((3,0),4), ((3,1),-1), ((3,2),-1), ((3,3),-1), ((3,4),-1), ((3,5),-1),
                              ((4,0),-1), ((4,1),6), ((4,2),5), ((4,3),-1), ((4,4),-1), ((4,5),-1),
                              ((5,0),-1), ((5,1),-1), ((5,2),-1), ((5,3),-1), ((5,4),1), ((5,5),4)]


-- True = o bloco é preto e não editável
boolMatrix = array ((0,0),(5,5)) [((0,0),True), ((0,1),False), ((0,2),False), ((0,3),True), ((0,4),True), ((0,5),True),
                                  ((1,0),True), ((1,1),False), ((1,2),False), ((1,3),False), ((1,4),False), ((1,5),False),
                                  ((2,0),True), ((2,1),False), ((2,2),False), ((2,3),False), ((2,4),False), ((2,5),False),
                                  ((3,0),False), ((3,1),False), ((3,2),False), ((3,3),False), ((3,4),False), ((3,5),True),
                                  ((4,0),False), ((4,1),False), ((4,2),False), ((4,3),False), ((4,4),False), ((4,5),True),
                                  ((5,0),True), ((5,1),True), ((5,2),True), ((5,3),False), ((5,4),False), ((5,5),True)]



getRow :: Array (Int,Int) Int -> Int -> Int -> [Int]
getRow mat a 6 = [] --gambiarra pois n sei fazer condição de parada em haskell
getRow mat a b = if mat!(a,b) /= -1 then [mat!(a,b)] ++ getRow mat a (b+1)
                 else getRow mat a (b+1)

getColumn :: Array (Int,Int) Int -> Int -> Int -> [Int]
getColumn mat 6 b = []
getColumn mat a b = if mat!(a,b) /= -1 then [mat!(a,b)] ++ getColumn mat (a+1) b
                    else getColumn mat (a+1) b

-- solve :: Array (Int,Int) Int -> Array (Int,Int) Int -> Array (Int,Int) Int
-- solve matNum matBool =

--retorna uma lista de numeros que podem ser utilizados para um determinado quadrado
possibleNum :: Array (Int,Int) Int -> Int -> Int -> [Int]
possibleNum mat a b = [ n | n <- [1..6], not $ any (n==) (getRow mat a 0), not $ any (n==) (getColumn mat 0 b)]

-- verifica se um quadrado nunca foi alterado e não está em um quadrado preto
isBlank :: Array (Int,Int) Int -> Array (Int,Int) Bool -> Int -> Int -> Bool
isBlank matNum matBool a b = matNum!(a,b) == -1 && matBool!(a,b) /= True

isSequence :: [Int] -> Bool
isSequence list = (((sort list) !! (length list - 1)) - ((sort list) !! 0)) == length list - 1


getAreasRow :: Array (Int, Int) Bool -> Int -> Int -> [(Int, Int)]
getAreasRow mat a 6 = []
getAreasRow mat a b  | mat!(a,b) = getAreasRow mat a (b+1)
                     | otherwise = (a,b) : getAreasRow mat a (b+1)

getAreasColumn :: Array (Int, Int) Bool -> Int -> Int -> [(Int, Int)]
getAreasColumn mat 6 a = []
getAreasColumn mat a b  | mat!(a,b) = getAreasRow mat (a+1) b
                        | otherwise = (a,b) : getAreasRow mat (a+1) b                     

getSequenceRow :: [(Int, Int)] ->[[(Int, Int)]]
getSequenceRow ((a,b):(c,d):xs) | (a == c-1) = [(a,b)] : getSequenceRow ((c,d):xs)
                                | otherwise  = getSequenceRow ((c,d):xs)
getSequenceRow ((a,b):()) = []


main = do

    --pega os brancos de cada linha
    print (getSequenceRow(getAreasRow boolMatrix 0 0))
    print (getSequenceRow(getAreasRow boolMatrix 1 0))
    print (getSequenceRow(getAreasRow boolMatrix 2 0))
    print (getSequenceRow(getAreasRow boolMatrix 3 0))
    print (getSequenceRow(getAreasRow boolMatrix 4 0))
    print (getSequenceRow(getAreasRow boolMatrix 5 0))
    print("")
     --pega os brancos de cada coluna
    print (getAreasColumn boolMatrix 0 0)
    print (getAreasColumn boolMatrix 0 1)
    print (getAreasColumn boolMatrix 0 2)
    print (getAreasColumn boolMatrix 0 3)
    print (getAreasColumn boolMatrix 0 4)
    print (getAreasColumn boolMatrix 0 5)
    print("")
    
    print("")

    print (getColumn matrix 0 1)
    print $ possibleNum matrix 0 1
    print $ isSequence [4,3]
    print $ isSequence [2,5,4]
    print $ isSequence [1,3,2]