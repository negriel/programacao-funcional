--6
--a

data Exp a =
  Val a -- um numero
  | Add (Exp a) (Exp a) -- soma de duas expressoes
  | Sub (Exp a) (Exp a) -- subtração 
  | Mult (Exp a) (Exp a) -- multiplicacao
  | Pot (Exp a) (Exp a) -- potenciacao

avalia :: Floating a => Exp a -> a
avalia (Val x) = x
avalia (Add exp1 exp2) = (avalia exp1) + (avalia exp2)
avalia (Sub exp1 exp2) = (avalia exp1) - (avalia exp2) 
avalia (Mult exp1 exp2) = (avalia exp1) * (avalia exp2)
avalia (Pot  exp1 exp2) = (avalia exp1) ** (avalia exp2)

--b
expr1:: Num a => Exp a
expr1 = (Mult (Add (Val 3) (Val 12) ) (Pot (Sub (Val 15) (Val 5)) (Mult (Val 1) (Val 3))))

expr2:: Num a => Exp a
expr2 = ( Sub ( Val 0 )( Mult ( Add  ( Add ( Val 6 ) ( Val 8 ) ) ( Sub ( Val 1 ) ( Val 5 ) ) ) ( Add (Val 2) (Pot (Val 6) (Val 2)))))

-- exercicio 7

data Hora = AM Int Int
          | PM Int Int
          deriving (Show, Eq, Ord)

--a
intervalo_minutos:: Int -> Bool
intervalo_minutos x
  |(x>=0) && (x<= 59) = True
  |otherwise = False 

intervalo_horas:: Int -> Bool
intervalo_horas x
  |(x>0) && (x<= 11) = True
  |otherwise = False

horas_decorridas:: Hora -> Int
horas_decorridas (AM hora minutos)
  |intervalo_horas (hora) == True && intervalo_minutos (minutos) == True = hora
  |otherwise = undefined
horas_decorridas (PM hora minutos)
  |intervalo_horas (hora) == True && intervalo_minutos (minutos) == True = hora + 12
  |otherwise = undefined

minutos_decorridos::Hora -> Int
minutos_decorridos (AM hora minutos)
  |intervalo_horas (hora) == True && intervalo_minutos (minutos) == True = minutos + hora * 60
  |otherwise = undefined
minutos_decorridos (PM hora minutos)
  |intervalo_horas (hora) == True && intervalo_minutos (minutos) == True = minutos + (hora + 12) * 60
  |otherwise = undefined

segundos_decorridos:: Hora -> Int
segundos_decorridos (AM hora minutos)
  |intervalo_horas (hora) == True && intervalo_minutos (minutos) == True = (minutos + hora * 60) *60
  |otherwise = undefined
segundos_decorridos (PM hora minutos)  
  |intervalo_horas (hora) == True && intervalo_minutos (minutos) == True = (minutos + (hora + 12) * 60) * 60
  |otherwise = undefined
  

-- exercicio 8
--a

type Data = (Int, Int, Int)

precede:: Data -> Data -> Bool
precede (d1,m1,a1) (d2,m2,a2)
  |(a1 < a2) = True
  |(a1 == a2) && (m1< m2) = True
  |(a1 == a2) && (m1 == m2) && (d1 < d2) = True
  |otherwise = False

data Contato = Nome String | Fone String
              deriving (Eq , Show)

data Mensagens = Registro Contato String Data Hora String
                deriving (Show)

banco_mensagens::[Mensagens]
banco_mensagens = 
  [(Registro (Nome "Nayara") "msg 1" (06, 10, 2020) (AM 4 34) "Facebook"),
  (Registro (Nome "Gabriel") "msg 2" (06 ,10, 2020) (PM 7 53) "Linkedin"),
  (Registro (Fone "1234") "msg 3" (06, 10, 2020) (PM 3 15) "Whatsapp"),
  (Registro (Nome "Nayara") "msg 4" (06, 10, 2020) (AM 5 34) "Facebook"),
  (Registro (Nome "Gabriel") "msg 5" (06, 10, 2020) (PM 8 53) "Linkedin"),
  (Registro (Fone "1234") "msg 6" (06, 10, 2020) (AM 3 15) "Whatsapp"),
  (Registro (Nome "Nayara") "msg 7" (06, 10, 2020) (PM 4 34) "Linkedin"),
  (Registro (Nome "Gabriel") "msg 8" (06, 10, 2020) (AM 7 53) "Whatsapp"),
  (Registro (Fone "1234") "msg 9" (06, 10, 2020) (PM 1 15) "Linkedin"),
  (Registro (Nome "Nayara") "msg 10" (06, 10, 2020) (AM 6 34) "Whatsapp"),
  (Registro (Nome "Gabriel") "msg 11" (06, 10, 2020) (PM 10 53) "Linkedin"),
  (Registro (Fone "1234") "msg 12" (06, 10, 2020) (AM 11 15) "Facebook"),
  (Registro (Nome "Nayara") "msg 13" (06, 10, 2020) (AM 9 34) "Facebook"),
  (Registro (Nome "Gabriel") "msg 14" (06, 10, 2020) (PM 7 21) "Facebook"),
  (Registro (Fone "1234") "msg 15 " (06, 10, 2020) (AM 3 39) "Linkedin"),
  (Registro (Nome "Nayara") "msg 1" (07 ,10, 2020) (AM 4 34) "Facebook"),
  (Registro (Nome "Gabriel") "msg 2" (07 ,10, 2020) (PM 7 53) "Linkedin"),
  (Registro (Fone "1234") "msg 3" (07 ,10, 2020) (PM 3 15) "Whatsapp"),
  (Registro (Nome "Nayara") "msg 4" (07 ,10, 2020) (AM 5 34) "Facebook"),
  (Registro (Nome "Gabriel") "msg 5" (07 ,10, 2020) (PM 8 53) "Linkedin"),
  (Registro (Fone "1234") "msg 6" (07 ,10, 2020) (AM 3 15) "Whatsapp"),
  (Registro (Nome "Nayara") "msg 7" (07 ,10, 2020) (PM 4 34) "Linkedin"),
  (Registro (Nome "Gabriel") "msg 8" (07 ,10, 2020) (AM 7 53) "Whatsapp"),
  (Registro (Fone "1234") "msg 9" (07 ,10, 2020) (PM 1 15) "Linkedin"),
  (Registro (Nome "Nayara") "msg 10" (07 ,10, 2020) (AM 6 34) "Whatsapp"),
  (Registro (Nome "Gabriel") "msg 11" (07 ,10, 2020) (PM 10 53) "Linkedin"),
  (Registro (Fone "1234") "msg 12" (07 ,10, 2020) (AM 11 15) "Facebook"),
  (Registro (Nome "Nayara") "msg 13" (07 ,10, 2020) (AM 9 34) "Facebook"),
  (Registro (Nome "Gabriel") "msg 14" (07 ,10, 2020) (PM 7 21) "Facebook"),
  (Registro (Fone "1234") "msg 15 " (07 ,10, 2020) (AM 3 39) "Linkedin")] 

--b
troca :: [Mensagens] -> [Mensagens]
troca [x] = [x]
troca (msg1 : msg2 : xs)
  | compara msg1 msg2 = msg2 : troca (msg1 : xs)
  | otherwise = msg1 : troca (msg2 : xs)
  where
    compara (Registro (Nome _) _ _ _ _) (Registro (Fone _) _ _ _ _) = True -- Ocorre troca, fone vem primeiro
    compara (Registro (Fone _) _ _ _ _) (Registro (Nome _) _ _ _ _) = False -- Não ocorre troca
    compara (Registro (Nome nome1) _ _ _ _) (Registro (Nome nome2) _ _ _ _) = nome1 > nome2
    compara (Registro (Fone nome1) _ _ _ _) (Registro (Fone nome2) _ _ _ _) = nome1 > nome2

bubblesort:: [Mensagens] -> Int -> [Mensagens]
bubblesort lista 0 = lista
bubblesort lista n = bubblesort (troca lista) (n-1)

ordena_contato:: [Mensagens] -> [Mensagens]
ordena_contato [] = []
ordena_contato lista = bubblesort lista (length lista)

--9
data ArvoreBinInt = Nulo |
                    No Int ArvoreBinInt ArvoreBinInt
                  deriving  (Show, Eq, Ord)

arv1 = No 6(No 2 Nulo Nulo)(No 7 (No 5 Nulo Nulo) (No 11 Nulo Nulo))

--a
internos:: ArvoreBinInt -> [Int]
internos Nulo = []
internos (No x Nulo Nulo) = []
internos (No x menores maiores) = [x] ++ internos menores ++ internos maiores

soma_nos:: ArvoreBinInt -> Int
soma_nos Nulo = 0
soma_nos (No x menores maiores) = x + (soma_nos menores) + (soma_nos maiores)

pertence:: Int -> ArvoreBinInt -> Bool
pertence _ Nulo = False
pertence x (No info menores maiores)
  | x == info = True
  | x < info = (pertence x menores)
  | otherwise = (pertence x maiores)

--exercicio 10
data ArvBinEA a = Vazia |
 Folha a |
 NoEA (Char, ArvBinEA a, ArvBinEA a)
 deriving (Show) 

ea::ArvBinEA Float
ea = NoEA ('+', NoEA ('*', Folha 10, Folha 5), Folha 7) 

opera_arvore:: Floating a => ArvBinEA a -> a
opera_arvore Vazia = 0
opera_arvore (Folha x) = x 
opera_arvore (NoEA (x, esquerda, direita))
    |x == '+' = (opera_arvore esquerda) + (opera_arvore direita)
    |x == '-' = (opera_arvore esquerda) - (opera_arvore direita)
    |x == '*' = (opera_arvore esquerda) * (opera_arvore direita)
    |x == '/' = (opera_arvore esquerda) * (opera_arvore direita)
    |x == '^' = (opera_arvore esquerda) ** (opera_arvore direita)
    |otherwise = undefined

