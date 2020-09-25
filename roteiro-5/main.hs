--1
--a
bissexto :: Int -> Bool
bissexto x = caso1 || (caso2 && caso3)
        where
         caso1 = (mod x 400 == 0)
         caso2 = (mod x 4 == 0)
         caso3 = (mod x 100 /= 0)

type Data = (Int, Int, Int)

valida :: Data -> Bool 
valida (d,m,a) = caso1 || caso2 || caso3 || caso4
      where
        caso1 = d >= 1 && d <= 31 && (m == 1 || m ==3 || m == 5 || m ==   7 || m == 8 || m == 10 || m == 12)
        caso2 = d >= 1 && d <= 30 && (m == 4 || m ==6 || m == 9 || m ==   11)
        caso3 = d >= 1 && d <= 28 && m == 2 && not (bissexto a)
        caso4 =  d >= 1 && d <= 29 && m ==2 && (bissexto a)

--b

bissextos:: [Int] -> [Int]
bissextos x = lista
  where
    lista = [ n | n <- x, bissexto n == True]

--c
type Emprestimo = (String, String, Data, Data, String)
type Emprestimos = [Emprestimo]
bdEmprestimo::Emprestimos
bdEmprestimo =
 [("H123C9","BSI945",(12,9,2009),(20,09,2009),"aberto"),
 ("L433C5","BCC021",(01,9,2009),(10,09,2009),"encerrado"),
 ("M654C3","BCC008",(04,9,2009),(15,09,2009),"aberto")]

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
emp_atrasados lista datahoje = atrasados
      where
        atrasados = [x | x <- lista, (emprestimoemdia datahoje x)]

--d
fibo2 :: Int -> (Int, Int)
fibo2 0 = (0,1)
fibo2 n = passo (fibo2 (n-1))
    where
      passo :: (Int, Int) -> (Int, Int)
      passo (x, y) = (y, x+y)

--e
fatorialp::Int->Int
fatorialp x = prodIntervalo 1 x
      where
        prodIntervalo:: Int-> Int ->Int
        prodIntervalo m n
          |m == n = n
          |m<n = m*(prodIntervalo (m+1) (n))


--2
--a
bissexto_let :: Int -> Bool
bissexto_let x = let 
                  caso1 = (mod x 400 == 0) 
                  caso2 = (mod x 4 == 0)
                  caso3 = (mod x 100 /= 0)
                  in 
                  caso1 || (caso2 && caso3)



valida_let :: Data -> Bool 
valida_let (d,m,a) = let
                  caso1 = d >= 1 && d <= 31 && (m == 1 || m ==3 || m == 5 || m ==   7 || m == 8 || m == 10 || m == 12)
                  caso2 = d >= 1 && d <= 30 && (m == 4 || m ==6 || m == 9 || m ==   11)
                  caso3 = d >= 1 && d <= 28 && m == 2 && not (bissexto_let a)
                  caso4 =  d >= 1 && d <= 29 && m ==2 && (bissexto_let a)
                  in
                  caso1 || caso2 || caso3 || caso4

--b

bissextos_let:: [Int] -> [Int]
bissextos_let x = let
                  lista = [ n | n <- x, bissexto n == True]
                  in
                  lista

--c
emp_atrasados_let :: Emprestimos -> Data -> Emprestimos
emp_atrasados_let lista datahoje =  let
                                atrasados = [x | x <- lista, (emprestimoemdia datahoje x)]
                                in
                                atrasados
--d
fibo2_let :: Int -> (Int, Int)
fibo2_let 0 = (0,1)
fibo2_let n = let
          passo :: (Int, Int) -> (Int, Int)
          passo (x, y) = (y, x+y)
          in 
          passo (fibo2 (n-1))

--e
fatorialp_let::Int->Int
fatorialp_let x = let 
              prodIntervalo:: Int-> Int ->Int
              prodIntervalo m n
                |m == n = n
                |m<n = m*(prodIntervalo (m+1) (n))
              in
              prodIntervalo 1 x

   