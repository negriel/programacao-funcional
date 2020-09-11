{-Primeiro Trabalho de Programação Funcional – Modalidade AARE
ALUNOS: Gabriel Teodoro Ribeiro 11911BCC013
        Rafael Andrade Prado | 11821BCC012
-}

-- Exercicio 1
triangulo:: Int -> Int -> Int -> String
triangulo x y z
    | (x ==0) || (y==0) || (z==0) = "nao eh um triangulo"
    |(x + y + z) /= 180 = "nao eh um triangulo"
    |(x == y) && (y == z) && (x + y + z == 180) = "o triangulo eh equilatero"
    |(x== 90 && y+z ==90) || (y == 90 && x+z == 90) || (z==90 && x+y ==90) && (x + y + z ==180) = "o triangulo eh retangulo"
    |(x>90 && y+z <90 ) || (y > 90 && x+z < 90) || (z>90 && x+y <90) && (x + y + z ==180) = "o triangulo eh obtuso"
    | otherwise = "o triangulo eh simples"

--2
delta:: Float -> Float -> Float -> Float
delta a b c = (b^2 -(4*a*c))
primeirograu:: Float -> Float -> Float
primeirograu b c = -c/b

equacao:: Float -> Float -> Float -> (Float,Float)
equacao a b c
    | a/=0 = ((-b + sqrt(delta a b c))/ (2*a) ,(-b - sqrt (delta a b c))/ (2*a))
    |otherwise = ((primeirograu b c), a) 
    
--3 
type Data = (Int, Int, Int)
type Passagem = Float
valor_passagem:: Passagem -> Data -> Data -> Float
valor_passagem x (da,ma,aa) (dn,mn,an)
    |(da <= dn) && (ma <= mn) && (aa - an <= 2)= 0.15 * x
    |(da > dn) && (ma < mn) && (aa - an <= 2)= 0.15 * x
    |(da <= dn) && (ma <= mn) && (aa - an <= 10)= 0.4 * x
    |(da > dn) && (ma < mn) && (aa - an <= 10)= 0.4 * x
    |(da >= dn) && (ma >= mn) && (aa - an >= 70)= 0.5 * x
    |(da < dn) && (ma > mn) && (aa - an >= 70)= 0.5 * x
    |otherwise = x



--4

lista :: [Int]
lista = [1..15]

-- a) gera1: gerar a lista de inteiros, contendo o quadrado de todos os ímpares entre 4 e 14.

impar :: Int -> Bool
impar x = mod x 2 /= 0

gera1 :: [Int]
gera1 = [x^2 | x <- lista, impar x, x > 4 && x < 14]

-- b) gera2: gerar a lista de duplas formadas tendo o primeiro elemento entre 1 e 4  e o segundo elemento no intervalo fechado entre o valor do primeiro elemento e o seu dobro.

gera2 :: [(Int,Int)]
gera2 = [(x,y) | x <- lista, y <- lista, x > 1 && x < 4, y >= x && y <= x * 2]

-- c) gera3: a partir de uma lista l1 entre 10 e 15, gerar a lista com todos os elementos dentro do intervalo fechado definido entre 1 e cada elemento de l1 (Obs.: pode ter elemento repetido na lista final).

l1 :: [Int]
l1 = [11..14]

gera3 :: [Int]
gera3 = [x | x <- lista, x >= 1 && x <= l1!!0] ++ [x | x <- lista, x >= 1 && x <= l1!!1] ++ [x | x <- lista, x >= 1 && x <= l1!!2] ++ [x | x <- lista, x >= 1 && x <= l1!!3]


-- d) gera4: gerar uma lista de duplas, onde cada dupla são 2 números consecutivos de 1 a 16, sendo o primeiro elemento ímpar (Ex: (1,2) e (3,4))

gera4 :: [(Int, Int)]
gera4 = [(x, x+1)| x <- [1..16], mod x 2 /= 0 ]

-- e) gera5: a partir da lista de duplas geradas no item d, gerar a lista onde cada elemento corresponde à soma dos elementos da dupla.

gera5 :: [Int]
gera5 = [ x+y | (x,y) <- gera4]

-- 5a

multiplo2 :: Int -> Bool
multiplo2 x = mod x 2 == 0

funcao1 :: [Int] -> Int
funcao1 xs = length [x | x <- xs, x < 0 , multiplo2 x]


--b

funcao2 :: [Int] -> [Int]
funcao2 xs = [x | x <- xs, x < 0 , multiplo2 x]

--6

dist :: (Float,Float) -> Float
dist (x,y) = sqrt (x^2 + y^2)

distancia :: [(Float,Float)] -> [Float]
distancia xys = [dist (x,y) | (x,y) <- xys]

--7
fatores:: Int -> [Int]
fatores n = [ x | x <- [1..n],mod n x == 0]

primos:: Int -> Int -> [Int]
primos x y = [ z | z <- [x..y], fatores z == [1,z]]

--8
mdc::Int->Int->Int
mdc a b 
  | a < b = mdc b a 
  | b == 0 = a
  | otherwise = mdc b (mod a b)

mmc::Int->Int->Int
mmc x y = div (x * y)  (mdc x y) 

mmc3::Int -> Int -> Int -> Int
mmc3 x y z = mmc x (mmc y z)

--9
serie :: Float -> Int -> Float
serie x n
         | n == 1 = 1/x
         | even n = (x / fromIntegral (n)) + (serie x (n-1))
         | otherwise = (fromIntegral (n) / x) + (serie x (n-1))

--10

divide:: Int-> String
divide x
    |(mod x 3 == 0) && (mod x 5 == 0) = "fizzbuzz"    
    | mod x 3 == 0 = "fizz"
    | mod x 5 == 0 = "buzz"
    | otherwise = "no"


fizzbuzz:: Int -> [String]
fizzbuzz x = [divide x| x <- [1..x]]

--11

contaumelem:: Int -> [Int] -> Int
contaumelem x [] = 0
contaumelem x (y:resto)
    | x==y = 1 + contaumelem x resto
    | otherwise = contaumelem x resto

contaocorrencias:: Int -> Int -> [Int] -> (Int, Int)
contaocorrencias x y lista = (contaumelem x lista, contaumelem y lista)

--12
umaocorrencia:: Int -> [Int] -> Bool
umaocorrencia elem lista
  |contaumelem elem lista == 1 = True
  |otherwise = False


--13 
intercala:: [Int] -> [Int] -> [Int]
intercala lista1 [] =lista1
intercala [] lista2 = lista2
intercala (x:restox) (y:restoy) = x:y:intercala (restox) (restoy)

--14
type Contato = (String, String, String, String)

contatos :: [Contato]
contatos =   [ ("N1", "End1", "Tel1", "Email1"), ("N2", "End2", "Tel2", "Email2"),("N3", "End3", "Tel3", "Email3")]

recuperacontato:: [Contato] -> String -> String
recuperacontato [] email = "Email desconhecido"
recuperacontato ((nome, _, _, emailcadastro) : restocontatos) email
  | emailcadastro == email = nome
  | otherwise = recuperacontato restocontatos email

encontracontato:: String -> String
encontracontato emailprocurado = recuperacontato contatos emailprocurado

--15
type Pessoa = (String, Float, Int, Char)

pessoas :: [Pessoa]
pessoas =
  [ ("Joao", 1.85, 26, 'C'),
    ("Maria", 1.55, 62, 'S'),
    ("Jose", 1.78, 42, 'C'),
    ("Paulo", 1.93, 25, 'S'),
    ("Clara", 1.70, 33, 'C'),
    ("Bob", 1.45, 21, 'C'),
    ("Rosana", 1.58, 39, 'S'),
    ("Daniel", 1.74, 72, 'S'),
    ("Jocileide", 1.69, 18, 'S')
  ]


 --altura media lista 
todasalturas :: [Pessoa] -> Float
todasalturas [] = 0
todasalturas ((_, altura, _, _) : tail) = altura + (todasalturas tail)

alturamedia :: [Pessoa] -> Float
alturamedia lista = (todasalturas lista) / fromIntegral (length lista) :: Float

-- menor idade
todasidades:: [Pessoa] -> [Int]
todasidades [] = []
todasidades ((nome, altura, idade, estado): tail) = [i| i<-idade : todasidades tail]

menoridade:: [Pessoa] -> Int
menoridade lista = minimum (todasidades lista)  

--estado civil mais velho
maioridade:: [Pessoa] -> Int
maioridade lista = maximum (todasidades lista)

maisvelho :: [Pessoa] -> Int -> (String, Char)
maisvelho [] id1= ("", ' ')
maisvelho ((nome, _, idade, ecivil) : restolista) id1
  | id1 == idade = (nome, ecivil)
  | otherwise = maisvelho restolista id1

maisvelhocasado :: [Pessoa] -> (String, Char)
maisvelhocasado lista = maisvelho lista (maioridade lista)

--pessoas > 50
cinquenta lista =
  [(nome, altura, idade, estado) | (nome, altura, idade, estado) <- lista, idade >= 50]

--casados com mais de x
casadas lista x =
  [(nome, altura, idade, estado) | (nome, altura, idade, estado) <- lista, idade >= x, estado == 'C']

--16
insereord:: Int -> [Int] -> [Int]
insereord x [] = [x]
insereord elem (x:restox)
    | elem <= x = elem:x:restox
    |otherwise = x:insereord elem (restox)

--17
reverte:: [t]->[t]
reverte [] = []
reverte [t] =[t]
reverte (t:restot) = reverte (restot) ++ [t]


--18

existe :: (Eq a) => a -> [a] -> Bool
existe a [] = False
existe a (x:xs) = if a == x then True 
                            else existe a xs

sem_repetidos :: (Eq a) => [a] -> [a]
sem_repetidos [] = []
sem_repetidos (x:xs) = if existe x xs then sem_repetidos xs
                                      else x : sem_repetidos xs


--19 
notas :: [Int]
notas = [1,2,4,10]

troco:: Int -> [[Int]]
troco 0 = [[]]
troco valor = [v:vresto | v <- notas, valor >= v, vresto <-troco (valor-v)]

