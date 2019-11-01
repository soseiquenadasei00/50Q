import Data.Char
import Data.List


--1 (Apresente uma defini ̧c ̃ao recursiva da fun ̧c ̃ao (pr ́e-definida)enumFromTo ::  Int -> Int ->[Int]
--que constr ́oi a lista dos n ́umeros inteiros compreendidos entre dois limites)-- 

myenumFromTo :: Int -> Int -> [Int]
myenumFromTo x y | x == y = [x]
                 | x<y = x : (myenumFromTo (x + 1) y)
                 | x> y = x : (myenumFromTo (x-1) y)
                 | otherwise =[y]


--2  Apresente uma defini ̧c ̃ao recursiva da fun ̧c ̃ao (pr ́e-definida)enumFromThenTo ::  Int -> Int-> Int -> [Int]
--que constr ́oi a lista dos n ́umeros inteiros compreendidos entre dois limitese espa ̧cados de um valor constante

enumFromThenTo1 :: Int -> Int -> Int -> [Int]
enumFromThenTo1 x y z | x == y = [x]
                      | x >= z = []
                      | otherwise = (x :(enumFromThenTo y (2*y -x) z))

--3  Apresente  uma  defini ̧c ̃ao  recursiva  da  fun ̧c ̃ao  (pr ́e-definida)(++) ::  [a] -> [a] -> [a] que 
--concatena duas listas.Por exemplo,(++) [1,2,3] [10,20,30]corresponde `a lista[1,2,3,10,20,30].

mypospos :: [a] -> [a] -> [a]
mypospos [] x = x
mypospos x [] = x 
mypospos x y = (head x) : (mypospos (tail x) y)

--4  Apresente uma definição recursiva da função (pré-definida) (!!) :: [a] -> Int -> a que
--dada uma lista e um inteiro, calcula o elemento da lista que se encontra nessa posição (assume-
--se que o primeiro elemento sdon’t need to recover, and then restore.e encontra na posição 0).
--Por exemplo, (!!)
--[10,20,30] 1 corresponde a 20.

ind :: [a] -> Int -> a
ind (h:t) 0 = h
ind (h:t) i = ind t (i-1)

--5  Apresente uma definição recursiva da função (pré-definida) reverse :: [a] -> [a] que
--dada uma lista calcula uma lista com os elementos dessa lista pela ordem inversa.
--Por exemplo, reverse [10,20,30] corresponde a [30,20,10].

myreverse :: [a]-> [a]
myreverse [] = []
myreverse u = last u: myreverse (init u)


--6  Apresente uma definição recursiva da função (pré-definida) take :: Int -> [a] -> [a] que
--dado um inteiro n e uma lista l calcula a lista com os (no máximo) n primeiros elementos de l.
mytake :: Int -> [a] -> [a]
mytake n [] = []
mytake  0 l = l
mytake n (h:t) |n>0 = h : (take (n-1) t)

--7  Apresente uma definição recursiva da função (pré-definida) drop :: Int -> [a] -> [a] que
--dado um inteiro n e uma lista l calcula a lista sem os (no máximo) n primeiros elementos de l.
--Se a lista fornecida tiver n elementos ou menos, a lista resultante será vazia.
--Por exemplo, drop 2 [10,20,30] corresponde a [30].

mydrop :: Int -> [a] -> [a]
mydrop 0 l= l 
mydrop n [] = []
mydrop n (h:t) |n>0 = mydrop (n-1) t 

--8  Apresente uma definição recursiva da função (pré-definida) zip ::
--constói uma lista de pares a partir de duas listas.
--[a] -> [b] -> [(a,b)]
--Por exemplo, zip [1,2,3] [10,20,30,40] corresponde a [(1,10),(2,20),(3,30)].

myzip :: [a] -> [b] -> [(a,b)]
myzip (h:t) (x:xs) = (h,x) : (zip t xs)

--9  Apresente uma definição recursiva da função (pré-definida) elem ::
--Bool que testa se um elemento ocorre numa lista. Eq a => a -> [a] ->
--Por exemplo, elem 20 [10,20,30] corresponde a True enquanto que elem 2 [10,20,30] corresponde a False.

myelem :: Eq a => a-> [a]-> Bool
myelem n [] = False
myelem n (h:t) |n == h = True 
               |otherwise = myelem n t

--10  Apresente uma definição recursiva da função (pré-definida) replicate :: Int -> a ->
--[a] que dado um inteiro n e um elemento x constói uma lista com n elementos, todos iguais a x.
--Por exemplo, replicate 3 10 corresponde a [10,10,10].

myreplicate :: Int -> a -> [a]
myreplicate 0 _ = []
myreplicate n x |n>0 = x : myreplicate (n-1) x 

--11 Apresente uma definição recursiva da função (pré-definida) intersperse :: a -> [a] ->[a] que dado um elemento e uma lista, 
--constrói uma lista em que o elemento fornecido é intercalado entre os elementos da lista fornecida.
--Por exemplo, intersperce 1 [10,20,30] corresponde a [10,1,20,1,30].

myinterspece :: a -> [a] -> [a]
myinterspece n [] = []
myinterspece n (h:t) = (h : n : (myinterspece n t))

--12  Apresente uma definição recursiva da função (pré-definida) group :: Eq a => [a] -> [[a]] que agrupa 
--elementos iguais e consecutivos de uma lista.
--Por exemplo, group [1,2,2,3,4,4,4,5,4] corresponde a [[1],[2,2],[3],[4,4,4],[5],[4]].

{--aux:: Eq a => ([a], [a]) -> ([a], [a])
aux (l, []) = (l, [])
aux ((h1:t1), (h2:t2)) | h1 == h2   = aux ((h1:(h2:t1)), t2)
                        otherwise = ((h1:t1), (h2:t2))  

mygroup:: Eq a => [a] -> [[a]]
mygroup [] = []
mygroup (h:t) = e : (mygroup d)
    --where
        (e, d) = aux ([h], t)
--}

mygroup:: Eq a => [a] -> [[a]]
mygroup [] = []
mygroup l@(h:t) = takeWhile (==h) l: mygroup (dropWhile (==h) l)


{--13   Apresente uma definição recursiva da função (pré-definida) concat ::
concatena as listas de uma lista.
[[a]] -> [a] que
Por exemplo, concat [[1],[2,2],[3],[4,4,4],[5],[4]] corresponde a [1,2,2,3,4,4,4,5,4].--}

myconcat :: [[a]]-> [a]
myconcat [] = []
myconcat (h:t) = h ++ myconcat t 

{--14   Apresente uma definição recursiva da função (pré-definida) inits ::
calcula a lista dos prefixos de uma lista.
[a] -> [[a]] que
Por exemplo, inits [11,21,13] corresponde a [[],[11],[11,21],[11,21,13]].--}

myinits :: [a] -> [[a]]
myinits [] = [[]]
myinits l =  myinits (init l) ++ [l]

{--15  Apresente uma definição recursiva da função (pré-definida) tails ::
calcula a lista dos sufixos de uma lista.
[a] -> [[a]] que
Por exemplo, tails [1,2,3] corresponde a [[1,2,3],[2,3],[3],[]].--}

mytails :: [a] -> [[a]]
mytails [] = [[]]
mytails l = [l] ++ mytails (tail l)

{--16   Apresente uma definição recursiva da função (pré-definida) isPrefixOf ::
-> [a] -> Bool que testa se uma lista é prefixo de outra.
Eq a => [a]
Por exemplo, isPrefixOf [10,20] [10,20,30] corresponde a True enquanto que isPrefixOf
[10,30] [10,20,30] corresponde a False.--}


myisPrefix0f :: Eq a => [a]-> [a]->Bool
myisPrefix0f [] _ = True
myisPrefix0f _ [] = False
myisPrefix0f (h:t) (x:xs) |h == x = myisPrefix0f t xs
                          |h /= x = False
                           

{--17  Apresente uma definição recursiva da função (pré-definida) isSuffixOf ::
-> [a] -> Bool que testa se uma lista é sufixo de outra.
Eq a => [a]
Por exemplo, isSuffixOf [20,30] [10,20,30] corresponde a True enquanto que isSuffixOf
[10,30] [10,20,30] corresponde a False.--}

myisSuffix0f :: Eq a => [a] -> [a]-> Bool 
myisSuffix0f [] _ = True 
myisSuffix0f _ [] = False
myisSuffix0f h x@(y:ys) = h == x || myisSuffix0f h ys


{--18 Apresente uma definição recursiva da função (pré-definida) isSubsequenceOf :: Eq a =>
[a] -> [a] -> Bool que testa se os elementos de uma lista ocorrem noutra pela mesma
ordem relativa.
Por exemplo, isSubsequenceOf [20,40] [10,20,30,40] corresponde a True enquanto que
isSubsequenceOf [40,20] [10,20,30,40] corresponde a False.--}


myisSubsequence0f :: Eq a => [a] -> [a]-> Bool
myisSubsequence0f [] _ = True
myisSubsequence0f _ [] = False
myisSubsequence0f (h:t) (x:xs) | h == x = myisSubsequence0f t xs 
                               | otherwise = myisSubsequence0f (h:t) xs 



{--19 Apresente uma definição recursiva da função (pré-definida) elemIndices :: Eq a => a ->
[a] -> [Int] que calcula a lista de posições em que um dado elemento ocorre numa lista.
Por exemplo, elemIndices 3 [1,2,3,4,3,2,3,4,5] corresponde a [2,4,6]--}
myelemIndices :: Eq a => a-> [a] -> [Int]
myelemIndices  _ [] = []
myelemIndices x l = aux 0 x l
    where
        aux _ _ [] = []
        aux n x (h:t) = if x == h
                         then n:aux (n + 1) x t 
                         else aux (n + 1) x t

{--20 Apresente uma definição recursiva da função (pré-definida) nub :: Eq a => [a] -> [a] que
calcula uma lista com os mesmos elementos da recebida, sem repetições.
Por exemplo, nub [1,2,1,2,3,1,2] corresponde a [1,2,3].--}


mynub :: Eq a => [a] -> [a]
mynub [] = [] 
mynub (h:t) = h : mynub (remove h t) 
   where 
    remove _ [] = []
    remove a (h:t) |a /=h =  h : remove a t 
                   |otherwise = remove a t 



{-- 21 Apresente uma definição recursiva da função (pré-definida) delete :: Eq a => a -> [a]
-> [a] que retorna a lista resultante de remover (a primeira ocorrência de) um dado elemento
de uma lista.
Por exemplo, delete 2 [1,2,1,2,3,1,2] corresponde a [1,1,2,3,1,2]. Se não existir nen-
huma ocorrência a função deverá retornar a lista recebida.--}


mydelete :: Eq a => a -> [a] -> [a]
mydelete _ [] = []
mydelete n (h:t) | n == h = t 
                 | otherwise = h : mydelete n t 

        

{--22 Apresente uma definição recursiva da função (pré-definida) (\\):: Eq a => [a] -> [a]
-> [a] que retorna a lista resultante de remover (as primeiras ocorrências) dos elementos da
segunda lista da primeira.
Por exemplo, (\\)[1,2,3,4,5,1] [1,5] corresponde a [2,3,4,1]--}

mybarrabarra :: Eq a => [a] -> [a] -> [a]
mybarrabarra l [] = l
mybarrabarra [] _ = []
mybarrabarra l (x:xs) = mybarrabarra (delete x l) xs    

{--23 Apresente uma definição recursiva da função (pré-definida) union :: Eq a => [a] -> [a]
-> [a] que retorna a lista resultante de acrescentar à primeira lista os elementos da segunda
que não ocorrem na primeira.
Por exemplo, union [1,1,2,3,4] [1,5] corresponde a [1,1,2,3,4,5].--}

myunion :: Eq a => [a] -> [a] -> [a]
myunion l [] = l
myunion [] l = l
myunion (x:y) (a:b) = if x == a then a : myunion y b else x: myunion y (a:b)



{--24 Apresente uma definição recursiva da função (pré-definida) intersect :: Eq a => [a] ->
[a] -> [a] que retorna a lista resultante de remover da primeira lista os elementos que não
pertencem à segunda.
Por exemplo, intersect [1,1,2,3,4] [1,3,5] corresponde a [1,1,3].--}


 
myintersect :: Eq a => [a] -> [a] -> [a]
myintersect [] _ = []
myintersect _ [] = [] 
myintersect (x1:y1) l@(x2:y2) |elem x1 l = x1 : myintersect y1 l 
                              |otherwise = myintersect y1 l
 

{-- 25 Apresente uma definição recursiva da função (pré-definida) insert :: Ord a => a -> [a]
-> [a] que dado um elemento e uma lista ordenada retorna a lista resultante de inserir
ordenadamente esse elemento na lista.
Por exemplo, insert 25 [1,20,30,40] corresponde a [1,20,25,30,40].--}

myinsert :: Ord a => a -> [a] -> [a]
myinsert n [] = [n]
myinsert n (h:t) = if n> h then h: (myinsert n t) else n: (h: t)


{--26 Apresente uma definição recursiva da função (pré-definida) unwords ::
junta todas as strings da lista numa só, separando-as por um espaço.
[String] -> String que
Por exemplo, unwords ["Programacao", "Funcional"] corresponde a "Programacao Funcional".--}

myunwords :: [String] -> String 
myunwords [] = []
myunwords (x:xs)= x ++ (' ' : myunwords xs)


{--27 Apresente uma definição recursiva da função (pré-definida) unlines :: [String] -> String que
junta todas as strings da lista numa só, separando-as pelo caracter ’\n’.
Por exemplo, unlines ["Prog", "Func"] corresponde a "Prog\nFunc\n".Apresente uma definição recursiva da função (pré-definida) unlines :: [String] -> String que
junta todas as strings da lista numa só, separando-as pelo caracter ’\n’.
Por exemplo, unlines ["Prog", "Func"] corresponde a "Prog\nFunc\n".--}

myunlines :: [String] -> String
myunlines [] = " "
myunlines (x:xs) = x ++ "\n" ++ myunlines xs 


{--28 Apresente uma definição recursiva da função pMaior :: Ord a => [a] -> Int que dada
uma lista não vazia, retorna a posição onde se encontra o maior elemento da lista. As posições
da lista começam em 0, i.e., a função deverá retornar 0 se o primeiro elemento da lista for o
maior.--}


mypMaior :: Ord a => [a] -> Int 
mypMaior [x] = 0 
mypMaior (h:t)    | h>= (t!! mypMaior t) = 0  
                  | otherwise = 1+ mypMaior t
    
{--29 Apresente uma definição recursiva da função temRepetidos ::
testa se uma lista tem elementos repetidos.
Eq a => [a] -> Bool que
Por exemplo, temRepetidos [11,21,31,21] corresponde a True enquanto que temRepetidos
[11,2,31,4] corresponde a False.--}

mytemRepetido :: Eq a => [a] -> Bool 
mytemRepetido [] = False
mytemRepetido (x:xs) |elem x xs = True
                     |otherwise = mytemRepetido xs

{--30 Apresente uma definição recursiva da função algarismos ::[Char] -> [Char] que deter-
mina a lista dos algarismos de uma dada lista de caracteres.
Por exemplo, algarismos "123xp5" corresponde a "1235".--}

myalgarismo :: [Char] -> [Char]
myalgarismo [] = []
myalgarismo (h:t) | elem h ['0' .. '9'] =  h : myalgarismo t
                  | otherwise = myalgarismo t

{--31 Apresente uma definição recursiva da função posImpares :: [a] -> [a] que determina os
elementos de uma lista que ocorrem em posições ı́mpares. Considere que o primeiro elemento
da lista ocorre na posição 0 e por isso par.
Por exemplo, posImpares [10,11,7,5] corresponde a [11,5].--}

myposImpares :: [a] -> [a]
myposImpares [] = []
myposImpares [h] = []
myposImpares (h:t:x) = t : myposImpares x

{-- 32 Apresente uma definição recursiva da função posPares :: [a] -> [a] que determina os
elementos de uma lista que ocorrem em posições pares. Considere que o primeiro elemento da
lista ocorre na posição 0 e por isso par.
Por exemplo, posPares [10,11,7,5] corresponde a [10,7].--}

myposPares :: [a] -> [a]
myposPares [] =[]
myposPares [h] = [h]
myposPares (h:t:x) = h : myposPares x

{--33 Apresente uma definição recursiva da função isSorted ::
se uma lista está ordenada por ordem crescente.
Ord a => [a] -> Bool que testa
Por exemplo, isSorted [1,2,2,3,4,5] corresponde a True, enquanto que isSorted [1,2,4,3,4,5]
corresponde a False.--}

myisSorted :: Ord a => [a]->Bool
myisSorted [h] = True
myisSorted (h:t:x) = (h <= t) && myisSorted (t:x)


{--34 Apresente uma definição recursiva da função iSort :: Ord a => [a] -> [a] que calcula
o resultado de ordenar uma lista. Assuma, se precisar, que existe definida a função inser8m t
:: Ord a => a -> [a] -> [a] que dado um elemento e uma lista ordenada retorna a lista
resultante de inserir ordenadamente esse elemento na lista.--}


myiSort :: Ord a => [a]-> [a]
myiSort [] = []
myiSort [x] = [x]
myiSort (x:t) = insert x (myiSort t)


{-- 35 Apresente uma definição recursiva da função menor :: String -> String -> Bool que
dadas duas strings, retorna True se e só se a primeira for menor do que a segunda, segundo
a ordem lexicográfica (i.e., do dicionário)
Por exemplo, menor "sai" "saiu" corresponde a True enquanto que menor "programacao"
"funcional" corresponde a False.--}

myMenor :: String -> String ->Bool 
myMenor _ [] = False 
myMenor [] _ = True
myMenor (x:xs) (y:ys) | x > y = False
                      | x < y = True
                      |otherwise = myMenor xs ys  



{--36 Considere que se usa o tipo [(a,Int)] para representar multi-conjuntos de elementos de a.
Considere ainda que nestas listas não há pares cuja primeira componente coincida, nem cuja
segunda componente seja menor ou igual a zero.
Defina a função elemMSet :: Eq a => a -> [(a,Int)] -> Bool que testa se um elemento
pertence a um multi-conjunto.
Por exemplo, elemMSet ’a’ [(’b’,2), (’a’,4), (’c’,1)] corresponde a True enquanto
que elemMSet ’d’ [(’b’,2), (’a’,4), (’c’,1)] corresponde a False.--}


myelemMSet :: Eq a => a-> [(a,Int)]-> Bool
myelemMSet n [] = False 
myelemMSet n ((letra,numero):xs) | n == letra = True 
                                 | otherwise = myelemMSet n xs  



{--37 Considere que se usa o tipo [(a,Int)] para representar multi-conjuntos de elementos de a.
Considere ainda que nestas listas não há pares cuja primeira componente coincida, nem cuja
segunda componente seja menor ou igual a zero.
Defina a função lengthMSet ::
conjunto.
[(a,Int)] -> Int que calcula o tamanho de um multi-
Por exemplo, lengthMSet [(’b’,2), (’a’,4), (’c’,1)] corresponde a 7.--}

mylenghtMSet :: [(a,Int)]-> Int 
mylenghtMSet [] = 0
mylenghtMSet ((y,x):xs) = x + mylenghtMSet xs 


{--38 Considere que se usa o tipo [(a,Int)] para representar multi-conjuntos de elementos de a.
Considere ainda que nestas listas não há pares cuja primeira componente coincida, nem cuja
segunda componente seja menor ou igual a zero.
Defina a função converteMSet ::
lista dos seus elementos
[(a,Int)] -> [a] que converte um multi-conjuto na
Por exemplo, converteMSet [(’b’,2), (’a’,4), (’c’,1)] corresponde a "bbaaaac".--}


myconverteMSet :: [(a,Int)] -> [a]
myconverteMSet [] = []
myconverteMSet ((y,x):xs) | x == 0 = myconverteMSet xs 
                          | otherwise = y : myconverteMSet ((y,x-1) : xs) 



{--39 Considere que se usa o tipo [(a,Int)] para representar multi-conjuntos de elementos de a.
Considere ainda que nestas listas não há pares cuja primeira componente coincida, nem cuja
segunda componente seja menor ou igual a zero.
Defina a função insereMSet :: Eq a => a -> [(a,Int)] -> [(a,Int)] que acrescenta
um elemento a um multi-conjunto.
Por exemplo, insereMSet ’c’ [(’b’,2), (’a’,4), (’c’,1)] corresponde a [(’b’,2),
(’a’,4), (’c’,2)].--}

myinsereMSet :: Eq a => a -> [(a,Int)] -> [(a,Int)]
myinsereMSet a [] = [(a,1)]
myinsereMSet a ((x,y):xs) | a == x = (x,y+1):xs
                          | otherwise = (x,y) : myinsereMSet a xs 


{--40 Considere que se usa o tipo [(a,Int)] para representar multi-conjuntos de elementos de a.
Considere ainda que nestas listas não há pares cuja primeira componente coincida, nem cuja
segunda componente seja menor ou igual a zero.
Defina a função removeMSet :: Eq a => a -> [(a,Int)] -> [(a,Int)] que remove um
elemento a um multi-conjunto. Se o elemento não existir, deve ser retornado o multi-conjunto
recebido.
Por exemplo, removeMSet ’c’ [(’b’,2), (’a’,4), (’c’,1)] corresponde a [(’b’,2),
(’a’,4)]--}


myremoveMSet :: Eq a => a -> [(a,Int)] -> [(a,Int)]
myremoveMSet a [] = []
myremoveMSet a ((l,n):t)  | a == l && n > 1 = ((l,n-1) : t)
                          | a == l && n == 1  = t 
                          | otherwise = (l,n) : myremoveMSet  a t
                          


{-- 41 Considere que se usa o tipo [(a,Int)] para representar multi-conjuntos de elementos de a.
Considere ainda que nestas listas não há pares cuja primeira componente coincida, nem cuja
segunda componente seja menor ou igual a zero.
Defina a função constroiMSet :: Ord a => [a] -> [(a,Int)] dada uma lista ordenada
por ordem crescente, calcula o multi-conjunto dos seus elementos.
Por exemplo, constroiMSet "aaabccc" corresponde a [(’a’,3), (’b’,1), (’c’,3)].--}


myconstroiMSet :: Ord a => [a] -> [(a,Int)]
my0constroiMSet [] = [] 
myconstroiMSet [n]= [(n,1)]
myconstroiMSet (l:ls) = reverse (myinsereMSet l (myconstroiMSet ls))


{--42 Apresente uma definição recursiva da função pré-definida partitionEithers ::
a b] -> ([a],[b]) que divide uma lista de Either s em duas listas.--}


myPartitionEithers :: [Either a b] -> ([a],[b])
myPartitionEithers [] = ([],[])
myPartitionEithers ((Left x):t) = (x: rx, ry) 
                where (rx, ry) = myPartitionEithers t
myPartitionEithers ((Right x):t) =  ( rx , x: ry)
                 where (rx, ry) = myPartitionEithers t



{--43 Maybe -}

catMaybes :: [Maybe a] -> [a]
catMaybes [] = []
catMaybes ((Just h):t) = h : catMaybes t
catMaybes (Nothing:t) = catMaybes t 
 


{-- 44 Defina a função posicao :: (Int,Int) -> [Movimento] -> (Int,Int) que, dada uma
posição inicial (coordenadas) e uma lista de movimentos, calcula a posição final do robot
depois de efectuar essa sequência de movimentos.--}

data Movimento = Norte | Sul | Este | Oeste deriving Show 

myposicao :: (Int,Int) -> [Movimento] -> (Int,Int)
myposicao p [] = p
myposicao (x,y) (h:t) = myposicao (case h of Norte -> (x,y+1)
                                             Sul -> (x,y-1) 
                                             Oeste -> (x-1,y)
                                             Este -> (x+1,y)) t

{--45 Defina a função caminho :: (Int,Int) -> (Int,Int) -> [Movimento] que, dadas as posições
inicial e final (coordenadas) do robot, produz uma lista de movimentos suficientes para que o
robot passe de uma posição para a outra.--}

mycaminho :: (Int,Int) -> (Int,Int) -> [Movimento]
mycaminho (x1,y1) (x2,y2) | x1 < x2 = Este : mycaminho (x1+1,y1) (x2,y2)
                          | x2 > x1 = Oeste : mycaminho (x1-1,y1) (x2,y2)
                          | y2 > y1 = Norte : mycaminho (x1,y1+1) (x2,x2)
                          | y1 > y2 = Sul : mycaminho (x1,y1-1) (x2,y2)
                          | otherwise = []


{--46 Defina a função vertical :: [Movimento] -> Bool que, testa se uma lista de movimentos
só é composta por movimentos verticais (Norte ou Sul).--}

myvertical :: [Movimento] -> Bool 
myvertical [] = True 
myvertical (h:t) = case h of Oeste -> False 
                             Este -> False 
                             otherwise -> myvertical t 

{--47  Defina a função maisCentral :: [Posicao] -> Posicao que, dada uma lista não vazia de
posições, determina a que está mais perto da origem (note que as coordenadas de cada ponto
são números inteiros).--}

data Posicao = Pos Int Int deriving Show 

{--mymaisCentral :: [Posicao] -> Posicao
mymaisCentral [] = (0,0)
mymaisCentral (Pos x y, (Pos)) =--}
     

{--48
SOCORRO.2--}


{--49Defina a função mesmaOrdenada :: [Posicao] -> Bool que testa se todas as posições de
uma dada lista têm a mesma ordenada. --}

mymesmaOrdenada :: [Posicao] -> Bool
mymesmaOrdenada [(Pos x y)]= True 
mymesmaOrdenada ((Pos x1 y1) : (Pos x2 y2) :xs) = y1 == y2 && mymesmaOrdenada ((Pos x2 y2) : xs)

{--50 Defina a função interseccaoOK :: [Semaforo] -> Bool que testa se o estado dos semáforos
de um cruzamento é seguro, i.e., não há mais do que semáforo não vermelho--}

data Semaforo = Verde | Amarelo | Vermelho deriving Show
interseccaoOK :: [Semaforo] -> Bool 
interseccaoOK [] = True
interseccaoOK (h:t) | contarNaoVermelho (h:t) >= 2 = False
                    | otherwise = True
                    

contarNaoVermelho :: [Semaforo] -> Int
contarNaoVermelho [] = 0 
contarNaoVermelho (x:xs) = case x of Vermelho -> contarNaoVermelho xs 
                                     otherwise -> 1 + contarNaoVermelho xs

