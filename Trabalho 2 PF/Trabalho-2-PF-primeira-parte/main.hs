{- Segundo Trabalho de Programação Funcional
  Alunos: Gabriel Teodoro Ribeiro - 11911bcc013
          Nayara Terezinha Nunes- 11911BCC006
-}

l1=[1..1000]
l2=[1000,999..1]
l3=l1++[0]
l4=[0]++l2
l5=l1++[0]++l2
l6=l2++[0]++l1
l7=l2++[0]++l2
x1=[1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20]
x2=[20,19,18,17,16,15,14,13,12,11,10,9,8,7,6,5,4,3,2,1]
x3=[11,12,13,14,15,16,17,18,19,20,1,2,3,4,5,6,7,8,9,10]
x4=[10,9,8,7,6,5,4,3,2,1,20,19,18,17,16,15,14,13,12,11]
x5=[11,12,13,14,15,5,4,3,2,1,16,17,18,19,20,10,9,8,7,6]
x6=[1,12,3,14,5,15,4,13,2,11,6,17,8,19,20,10,9,18,7,16]
x7 = [20,8,2,11,13,3,7,18,14,4,16,10,15,1,9,17,19,12,5,6] 

-- Exercicio 1
--a

minimo:: (Ord a) => [a] -> a
minimo [] = undefined
minimo [a] = a
minimo (h:t)
    |h<= (minimo t) = h
    |otherwise = minimo t

remove:: (Ord a) => a -> [a] -> [a]
remove a [] = []
remove a (h:t)
    |a==h = t
    |otherwise = h:(remove a t)

selection:: (Ord a) => [a] -> [a]
selection [] = []
selection [x] =[x]
selection (xs) = [menor] ++ selection (remove menor (xs))
                            where menor = foldr1 (min) xs

--b
insere_ord:: (Ord a) => a-> [a] ->[a]
insere_ord a [] =[a]
insere_ord a (h:t)
    |a<=h = (a:h:t)
    |otherwise= h:(insere_ord a t)

insertion:: (Ord a) => [a] ->[a]
insertion = foldr insere_ord []

--c
quicksort:: (Ord a) => [a] ->[a]
quicksort [] =[]
quicksort (h:t) = quicksort (filter (<h) t) ++ [h] ++ quicksort (filter (>=h) t)

--exercício 2
-- Variação 1
troca :: (Ord a) => ([a], Int) -> ([a], Int)
troca ([x], flag) = ([x], flag)
troca ((x : y : xs), flag) =
  if x > y
    then adiciona (troca ((x : xs), 1)) y
    else adiciona (troca ((y : xs), flag)) x
  where
   adiciona (l, f) e = (e : l, f)

bubble_aux :: (Ord a) => ([a], Int) -> Int -> ([a], Int)
bubble_aux (l, flag) 0 = (l, flag)
bubble_aux (l, flag) n
  | flag == 0 = (l, flag)
  | otherwise = bubble_aux (troca (l, 0)) (n -1)

bubble1 :: (Ord a) => [a] -> [a]
bubble1 [] = []
bubble1 list = fst (bubble_aux (list, -1) (length list))

-- Variação 2
bubble2 :: (Ord a) => [a] -> [a]
bubble2 [] = []
bubble2 lst =
  let troca [x] = [x]
      troca (x : y : xs) =
        if x > y
          then y : troca (x : xs)
          else x : troca (y : xs)

      split lst = (take (length lst - 1) lst, drop (length lst - 1) lst)

      bubble [x] = [x]
      bubble l = (bubble haTrocar) ++ ultimoElm
        where
          listaMod = troca l
          (haTrocar, ultimoElm) = split listaMod
   in bubble lst

-- Variação 3
bubble3 :: (Ord a) => [a] -> [a]
bubble3 [] = []
bubble3 l =
  let adiciona (l, f) y = (y : l, f)
      split lst = (take (length lst - 1) lst, drop (length lst - 1) lst)

      troca ([x], flag) = ([x], flag)
      troca ((x : y : xs), flag) =
        if x > y
          then adiciona (troca ((x : xs), 1)) y
          else adiciona (troca ((y : xs), flag)) x

      bubble ([x], flag) = ([x], flag)
      bubble (lst, flag)
        | n_flag == 0 = (lst, flag)
        | otherwise = (fst (bubble (parte_a_trocar, 0)) ++ ultimo_elem, 0)
        where
          (lista_trocada, n_flag) = troca (lst, flag)
          (parte_a_trocar, ultimo_elem) = split lista_trocada
   in fst (bubble (l, -1))

-- COM CONTAGEM

-- Variação 1
bubble1_cont :: (Ord a) => [a] -> ([a], Int)
bubble1_cont [] = ([], 0)
bubble1_cont list = format (buble_aux_cont (list, -1, 0) (length list))
  where
    format (l, _, c) = (l, c)

troca_cont :: (Ord a) => ([a], Int, Int) -> ([a], Int, Int)
troca_cont ([x], flag, n) = ([x], flag, n)
troca_cont ((x : y : xs), flag, n) =
  if x > y
    then adiciona (troca_cont ((x : xs), 1, n + 1)) y
    else adiciona (troca_cont ((y : xs), flag, n + 1)) x
  where
   adiciona (l, f, c) e = (e : l, f, c)

buble_aux_cont :: (Ord a) => ([a], Int, Int) -> Int -> ([a], Int, Int)
buble_aux_cont (l, flag, c) 0 = (l, flag, c)
buble_aux_cont (l, flag, c) n
  | flag == 0 = (l, flag, c)
  | otherwise = buble_aux_cont (troca_cont (l, 0, c)) (n -1)

-- Variação 2
bubble2_cont :: (Ord a) => [a] -> ([a], Int)
bubble2_cont [] = ([], 0)
bubble2_cont lst =
  let adiciona (l, c) e = (e : l, c)

      troca ([x], c) = ([x], c)
      troca ((x : y : xs), c) =
        if x > y
          then adiciona (troca (x : xs, c + 1)) y
          else adiciona (troca (y : xs, c + 1)) x

      split lst = (take (length lst - 1) lst, drop (length lst - 1) lst)

      bubble :: (Ord a) => ([a], Int) -> ([a], Int)
      bubble ([x], c) = ([x], c)
      bubble (l, c) = (proxima_etapa ++ ultimo_elem, rec_c)
        where
          (lista_trocada, c1) = (troca (l, c))
          (parte_a_trocar, ultimo_elem) = split lista_trocada
          (proxima_etapa, rec_c) = bubble (parte_a_trocar, c1)
   in bubble (lst, 0)

-- Variação 3
bubble3_cont :: (Ord a) => [a] -> ([a], Int)
bubble3_cont [] = ([], 0)
bubble3_cont l =
  let adiciona (l, f, c) y = (y : l, f, c)
      split lst = (take (length lst - 1) lst, drop (length lst - 1) lst)
      format (l, _, c) = (l, c)

      troca ([x], flag, c) = ([x], flag, c)
      troca ((x : y : xs), flag, c) =
        if x > y
          then adiciona (troca ((x : xs), 1, c + 1)) y
          else adiciona (troca ((y : xs), flag, c + 1)) x

      bubble ([x], flag, c) = ([x], flag, c)
      bubble (lst, flag, c)
        | n_flag == 0 = (lst, flag, c)
        | otherwise = (proxima_etapa ++ ultimo_elem, 0, rec_c)
        where
          (lista_trocada, n_flag, c1) = troca (lst, flag, c)
          (parte_a_trocar, ultimo_elem) = split lista_trocada
          (proxima_etapa, _, rec_c) = bubble (parte_a_trocar, 0, c1)
   in format (bubble (l, -1, 0))


{- A diferença no tempo de execução entre as implementações não foi relativamente grande, considerando a lista com 1000 elementos. Porém, a primeira aplicação realiza bem mais operações que a segunda e terceira variação. Como a segunda aplicação tem uma implementação relativamente mais simples que a terceira, elegemos a segunda como a melhor versão.-}

--exercício 3
--primeira variação
selection1:: (Ord a) => [a] -> [a]
selection1 [] = []
selection1 [x] =[x]
selection1 (xs) = menor : selection1 (remove menor (xs))
                            where menor = foldr1 (min) xs

--segunda variação

remove_menor:: (Ord a) => (a, [a]) ->(a, [a])
remove_menor (x, [y]) =  if x<y then (x,[y]) else (y,[x])
remove_menor (menor, (h:t))
    | h < menor = adiciona menor (remove_menor (h,t))
    | otherwise = adiciona h (remove_menor (menor,t))
    where 
      adiciona elemento (n,l) = (n,elemento:l)

selection2:: (Ord a) => [a] -> [a]
selection2 [] = []
selection2 [x] =[x]
selection2 (xs) = menor : (selection2 ultimo)
                            where (menor,ultimo) = remove_menor (head xs, tail xs)

--segunda variação com contador

remove_menor_cont:: (Ord a) => (a, [a], Int) ->(a, [a], Int)
remove_menor_cont (x, [y], c) =  if x<y then (x,[y],c + 1) else (y,[x], c + 1)
remove_menor_cont (menor, (h:t), cont)
    | h < menor = adiciona menor (remove_menor_cont (h,t, cont+1))
    | otherwise = adiciona h (remove_menor_cont (menor,t,cont+1))
    where 
      adiciona elemento (n,l,c) = (n,elemento:l,c)

selection2_cont:: (Ord a) => [a] -> ([a],Int)
selection2_cont [] = ([],0)
selection2_cont [x] =([x],0)
selection2_cont (xs) = (menor : repeticao, cont+ cont_rep)
                            where 
                              (menor,ultimo,cont) = remove_menor_cont (head xs, tail xs, 0)
                              (repeticao,cont_rep) = selection2_cont ultimo

{- Ao analisar o tempo de execução das implementações, a implementação da selection2 sem contador foi a de melhor desempenho. A versão com o contador também é uma boa versão porém tem o onus da implementação da função de remoção do menor elemento com a presença do contador que é relativamente complicada. O melhor desempenho da segunda implementação é devido ao fato de não ter que percorrer a lista duas vezes para encontrar o menor e para remove-lo. Sendo assim, eleita por nós como a melhor versão-}

--Exercicio 4

--variação 1
divide::(Ord a) => a -> [a] -> ([a],[a])
divide x [] = ([],[])
divide pivot [h] = if h<pivot then ([h],[]) else ([],[h])
divide pivot (h:t)
    | h < pivot = listamenores h (divide pivot t)
    |otherwise = listamaiores h (divide pivot t)
    where
      listamenores x (e,d) = (x:e,d)
      listamaiores x (e,d) = (e,x:d)

quicksort1:: (Ord a) => [a] ->[a]
quicksort1 [] =[]
quicksort1 (pivot:t) = (quicksort1 menores) ++ [pivot] ++ (quicksort1 maiores)
  where
    (menores, maiores) = divide pivot t

--variação 2
quicksort2 :: (Ord a) => [a] -> [a]
quicksort2 [] = []
quicksort2 lst = (quicksort2 menores) ++ [pivot] ++ (quicksort2 maiores)
      where
        tres_primeiros = take 3 lst
        pivot = if length (tres_primeiros) < 3 then tres_primeiros !! 0 else foldr1 (min) (tres_primeiros)
        deleta_repeticao _ [] = []
        deleta_repeticao x (y : ys)
          | x == y = ys
          | otherwise = y : deleta_repeticao x ys
        (menores, maiores) = divide pivot (deleta_repeticao pivot lst)

--com contador
divide_cont::(Ord a) => a -> [a] ->Int -> ([a],[a],Int)
divide_cont x [] n = ([],[],n)
divide_cont pivot [h] n = if h<pivot then ([h],[], n+1) else ([],[h], n+1)
divide_cont pivot (h:t) n
    | h < pivot = listamenores h (divide_cont pivot t (n+1))
    |otherwise = listamaiores h (divide_cont pivot t (n+1))
    where
      listamenores x (e,d,c) = (x:e,d,c)
      listamaiores x (e,d,c) = (e,x:d,c)

quicksort1_cont:: (Ord a) => [a] ->([a], Int)
quicksort1_cont [] =([],0)
quicksort1_cont (pivot:t) = (lista_menores ++ [pivot] ++ lista_maiores, n + c1 + c2)
  where
    (menores, maiores, n) = divide_cont pivot t 0
    (lista_menores, c1) = quicksort1_cont menores
    (lista_maiores, c2) = quicksort1_cont maiores

quicksort2_cont :: (Ord a) => [a] -> ([a], Int)
quicksort2_cont [] = ([], 0)
quicksort2_cont xs =
  let pivot = foldr1 (min) (take 3 xs)

      deleta_repeticao :: (Ord a) => a -> [a] -> Int -> ([a], Int)
      deleta_repeticao _ [] n = ([], n)
      deleta_repeticao x (y : ys) n
        | x == y = (ys, n + 1)
        | otherwise = adiciona y (deleta_repeticao x ys (n + 1))
        where
          adiciona e (l, c) = (e : l, c)

      (novo_ultimo, checagens) = deleta_repeticao pivot xs 0

      (esquerda, direita, n1) = divide_cont pivot novo_ultimo 0
      (lista_menores, c1) = quicksort2_cont esquerda
      (lista_maiores, c2) = quicksort1_cont direita
   in (lista_menores ++ [pivot] ++ lista_maiores, n1 + c1 + c2 + checagens + 3)       

   {- a segunda implementação da quicksort com contador tem um numero de operações consideravelmente mais baixas que as outras implementações. Em termos de desempenho, é a melhor versão. Porém, tem uma implementação complicada, que é um agravante no momento da escolha da melhor versão. A segunda versão da implementação também tem um bom desempenho mas a operação para encontrar o pivô se torna um pouco mais complexa. Apesar disso, escolhemos a segunda implementação como a melhor versão.-}


--Exercicio 5

merge :: (Ord a) => [a] -> [a] -> [a]
merge [] [] = []
merge l1 [] = l1
merge [] l2 = l2
merge (a : as) (b : bs)
  | a > b = b : (merge (a : as) bs)
  | otherwise = a : (merge as (b : bs))

mergesort :: (Ord a) => [a] -> [a]
mergesort [] = []
mergesort [x] = [x]
mergesort lst =
  let esquerda = mergesort (take ((length lst) `div` 2) lst)
      direita = mergesort (drop ((length lst) `div` 2) lst)
   in merge esquerda direita

-- Bucket Sort
bucket_aux :: Int -> Int -> Int -> Int -> [[Int]] -> [[Int]]
bucket_aux num k m n [bucket] =
  if ((num * k) `div` m) <= n
    then [num : bucket]
    else [bucket]
bucket_aux num k m n (bucket : buckets)
  | ((num * k) `div` m) <= n = (num : bucket) : buckets
  | otherwise = bucket : (bucket_aux num k m (n + 1) buckets)

bucketsort :: [Int] -> [Int]
bucketsort [] = []
bucketsort [x] = [x]
bucketsort l1 =
  let k = length l1

      m = foldr1 (max) l1

      buckets = [[] | _ <- [1 .. k]]

      novo_buckets = foldr (\x -> bucket_aux x k m 1) buckets l1

      separa_buckets = map (mergesort) novo_buckets

      finalList = foldr1 (++) separa_buckets
   in finalList
   
{- considerando o desempenho desses novos algoritimos e, também, dos implementados anteriormente, o quicksort continua sendo o algoritimo eleito por nós como o mais eficiente para a maioria das listas e que se adapta melhor ao haskell e sua recursividade-} 