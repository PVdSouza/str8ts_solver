import Data.Arrays


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