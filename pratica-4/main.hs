--1

lst1 = [x*2 | x <- [1..10], x*2 >= 12]-- [12,14,16,18,20]
lst2 = [ x | x <- [50..100], mod x 7 == 3] -- [52,59,66,73,80,87,94]
lst3 = [ x | x <- [10..20], x /= 13, x /= 15, x /= 19] --[10,11,12,14,16,17,18,20]
lst4=[(x,y)| x <- [1..4], y <- [x..5]]-- [(1,1),(1,2),(1,3),(1,4),(1,5),(2,2),(2,3),(2,4),(2,5),(3,3),(3,4),(3,5),(4,4),(4,5)]


--2
quadrados:: Int -> Int -> [Int]
quadrados x y = [ z^2 | z <-[x..y]]

--3
seleciona_impares:: [Int] -> [Int]
seleciona_impares lista = [l | l<- lista, not (even l)]

--4
tabuada:: Int -> [Int]
tabuada x= [ x*n | n <- [1..10]]

--5
bissexto :: Int -> Bool
bissexto x | (mod x 400 == 0) = True
           | (mod x 4 == 0) && (mod x 100 /= 0) = True
           | otherwise = False

bissextos:: [Int] -> [Int]
bissextos x = [ n | n <- x, bissexto n == True]

--6
sublista :: [[t]] -> [t]
sublista l1 = [x | elem<-l1, x<-elem]


--7
type Data = (Int, Int, Int)
type Emprestimo = (String, String, Data, Data, String)
type Emprestimos = [Emprestimo]
bdEmprestimo::Emprestimos
bdEmprestimo =
 [("H123C9","BSI945",(12,9,2009),(20,09,2009),"aberto"),
 ("L433C5","BCC021",(01,9,2009),(10,09,2009),"encerrado"),
 ("M654C3","BCC008",(04,9,2009),(15,09,2009),"aberto")]



valida :: Data -> Bool 
valida (d,m,a)
  | d >= 1 && d <= 31 && (m == 1 || m ==3 || m == 5 || m ==   7 || m == 8 || m == 10 || m == 12) = True
  | d >= 1 && d <= 30 && (m == 4 || m ==6 || m == 9 || m ==   11) = True
  | d >= 1 && d <= 28 && m == 2 && not (bissexto a) = True
  | d >= 1 && d <= 29 && m ==2 && (bissexto a) = True
  | otherwise = False 


precede::Data -> Data -> Bool
precede (d1,m1,a1) (d2,m2,a2)
  |not (valida (d1,m1,a1)) = False
  |not (valida (d2,m2,a2)) = False
  | (a1 < a2) = True
  | (a1 == a2) && (m1 < m2) = True
  | (a1 == a2) && (m1 == m2) && (d1 <= d2) = True
  |otherwise = False

emprestimoemdia:: Data -> Emprestimo ->Bool
emprestimoemdia datahoje (codigo, aluno, dataEmprestimo, devolucao, status) = precede devolucao datahoje

emp_atrasados :: Emprestimos -> Data -> Emprestimos
emp_atrasados lista datahoje = [x | x <- lista, (emprestimoemdia datahoje x)]

--8
npares:: [Int] -> Int
npares [] = 0
npares (h:tail)
    | mod h 2 ==0 = 1+ npares tail
    |otherwise = npares tail

--9
produtorio:: [Int] -> Int
produtorio [] = 0
produtorio [x] = x
produtorio (h:tail) = h * produtorio tail

--10--
sublistas::[[Int]]->[Int]
sublistas [] = []
sublistas (h:tail) = h ++ sublistas tail


--11
tamanho:: [t] -> Int
tamanho [] =0
tamanho [t] = 1
tamanho (h:tail) = 1 + tamanho tail

--12
uniaoNRec:: [Int] ->[Int] -> [Int]
uniaoNRec l1 l2 = [x | x<- l1 ] ++ [y | y<- l2 , elem y l1== False]


