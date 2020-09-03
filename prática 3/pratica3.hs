--1 A
ouA:: Bool -> Bool -> Bool
ouA True True = True
ouA True False = True
ouA False True = True
ouA False False = False

ouB :: Bool -> Bool -> Bool
ouB _ True = True
ouB True _ = True
ouB _ _ = False

ouC :: Bool -> Bool -> Bool
ouC True _ = True
ouC False c = c

--b

ou1:: Bool -> Bool -> Bool
ou1 x y
  | x == True = True
  | y == True = True
  | otherwise = False

ou2:: Bool -> Bool -> Bool
ou2 x y
    | x == False && y == False = False
    | otherwise = True


--2

type Ponto = (Float, Float)
distancia:: Ponto -> Ponto -> Float
distancia (xa,ya) (xb,yb) = sqrt ((xb-xa)^2 + (yb-ya)^2)

--4

fatorial_rec:: Int -> Int
fatorial_rec 0 = 1 --caso base
fatorial_rec x = x * fatorial_rec (x -1)

fatorial_g :: Int -> Int
fatorial_g x
  | x==0 = 1
  | x > 0 = x * fatorial_g (x-1)

--5

fibo:: Int -> Int
fibo 1 = 1
fibo 2 = 1
fibo n = fibo(n-2) + fibo(n-1)

--6

triang:: Int -> Int
triang 1 = 1
triang n = triang(n-1) + n

--7

passo :: (Int, Int) -> (Int, Int)
passo (x, y) = (y, x+y)

fibo2 :: Int -> (Int, Int)
fibo2 0 = (0,1)
fibo2 n = passo (fibo2 (n-1))


--8

potencia:: Int -> Int
potencia 1 = 2
potencia n = potencia (n-1) * 2

--9
prodIntervalo:: Int-> Int ->Int
prodIntervalo m n
  |m == n = n
  |m<n = m*(prodIntervalo (m+1) (n))

fatorialp::Int->Int
fatorialp x = prodIntervalo 1 x

--11
resto_div::Int->Int->Int
resto_div m n 
  |n>(m-n) = (m-n)
  |otherwise = resto_div (m-n) n

div_inteira::Int->Int->Int
div_inteira m n 
  |m==n = 1
  |otherwise = (resto_div m n)*n


--12

mdcg:: (Int, Int)-> Int
mdcg (m,n)
  | n==0 = m
  | otherwise = mdcg(n, (mod m n)) 

mdcp:: (Int, Int) -> Int
mdcp (m,0) = m
mdcp (m,n) = mdcp (n, (mod m n))

--13

binog::(Int, Int) -> Int
binog (n,k)
  | k==0 = 1
  | k==n = 1
  | otherwise = binog (n-1,k) + binog(n-1,k-1)

binomialc:: (Int,Int) -> Int
binomialc (n,0) = 1
binomialc (n,k) = if(k==n)
    then 1
    else binomialc (n-1,k) + binomialc (n-1,k-1)
--15
listaint:: Int -> Int -> [Int]
listaint a b
  |a==b = a:[]
  |a>b = []
  |otherwise = [a..b]

listapares:: Int -> Int -> [Int]
listapares a b
  |a==b || a+1 ==b || a>b = []
  |mod a 2 == 0 = listapares(a+1) b
  |otherwise = a+1:(listapares(a+1)b)