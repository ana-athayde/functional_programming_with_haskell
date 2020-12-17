head' (x:xs) = x

tail' (x:xs) = xs

init' (x:[]) = []
init' (x:xs) = x : init' xs

last' (x:[]) = x
last' (x:xs) = last'xs

-- Construtores de dados => Lista	
-- :	= Cria uma lista com elementos
-- []  = Cria uma lista vazia

-- Exercicio 1: Declare uma função que retorne o maior
-- valor de uma lista.
-- Exemplo: maior[10,3,5,3,12] =>12

maior [x] = x
maior (x:xs) = if x > maior xs then x else maior xs

--maior [10] = 10
--maior 10:4:12:3:[] = 12
-- 		10 > maior 4:12:3:[]
--		4  > maior 12:3:[]
--		12 > 3:[]
-- 		3


-- 		if 10 > maior 4:12:3:[] then 10 else maior 4:12:3:[]
--		if 4  > maior 12:3:[] then 4 else maior 12:3:[]
--		if 12 > 3:[] then 12 else maior 3:[]
--		3

-- 		if 10 > 12 then 10 else 12
--		if 4 > 12 then 4 else 12
--		if 12 > 3 then 12 else 3
--		3


-- Revisão
nprimeiros 0 _ = []
nprimeiros _ [] = []
nprimeiros n (x:xs) = x : nprimeiros (n-1) xs

-- nprimeiros 3 [1,2,3,4,5,6] ===> [1,2,3]
-- 1 : nprimeiros 2 [2,3,4,5,6]
-- 	2 : nprimeiros 1 [3,4,5,6]
--   3 : nprimeiros 0 [4,5,6]
-- 1:2:3:[] == [1,2,3]

-- Exercicio 2: Declare uma função que retorne os n últimos 
-- elementos de uma lista.
-- Exemplo: nultimos 3 [1,2,3,4,5,6] => [4,3,5]
-- length, reverse 

n-1

nultimos 0 _ = []
nultimos _ [] = []
nultimos n (x:xs) | lenght (x:xs) <= (x:xs)
				  | otherwise = nultimos n xs
				  
-- nultimos 3 [1,2,3,4,5,6]
-- nultimos 3 [2,3,4,5,6]
-- nultimos 3 [3,4,5,6]
-- nultimos 3 [4,5,6]
-- [4,5,6]

-- nultimos n ls@(x:xs) | lenght (x:xs) <= (x:xs)
--				  | otherwise = nultimos n xs

-- Exercicio 3: Declare uma função que retorne o inverso de uma lista
-- Exemplo: inverso[1,2,3,4] =>[4,3,2,1]

inverso :: [a] - [a]
inverso [] = []
inverso lista = last ls : inverso (init ls) 

-- inverso 1:2:3:4:[]
-- 	4:inverso(1:2:3:[])
-- 	3:inverso(1:2:[])
-- 	2:inverso(1:[])
-- 	1:inverso([])
-- 	[]
	
-- >> 4:3:2:1:[] == []

-- ++
-- 1:2:3:[] == [1,2,3] : [4,5,6] Não funciona porque esse operador só permite um numero antes dos dois pontos
-- por isso utilizamos o ++, porque permite uma lista antes e uma depois
-- : elemento > lista
-- ++ lista > lista

inverso' [] = []
inverso' (x:xs) = inverso xs ++ [x]

-- inverso' [1,2,3,4]
-- (inverso' [2,3,4] ++ [1])
-- (inverso' [3,4] ++ [2])
-- (inverso' [4] ++ [3])
-- (inverso' [] ++ [4])
-- []

{- [4] ++ [3] ++ [2] ++ [1] == [4,3,2,1] -}

-- Exercício 4: Reescreva a função nultimos utilizando a função inverso.

nultimos' n xs = inverso (take n (inverso xs))

{-2 [1,2,3,4]
	inverso xs>> [4,3,2,1]
	take 2 xs >> [4,3]
	inverso >> [3,4]
	
take-}

-- Exercicio 5: Exemplo: repetir 4 10 => [10,10,10,10]
-- replicate

repetir :: Int -> a -> [a]
repetir 0 _ = []
repetir n m = m : repetir (n-1) m

{-
repetir 4 10
	10 : repetir (3) 10
	10 : repetir (2) 10
	10 : repetir (1) 10
	10 : repetir (0) 10
	[]
	
>> 10:10:10:10:[]
-} 

-- Exercicio 6: Dobro de todos os números entre 1 e 100. Compreensão de listas.

f1 [] = []
f1 (x:xs) = x*2 : f1 xs
-- f1 [1..100]

f2 = [x*2 | x <- [1..100]] 
-- f2 [1..100]

-- Dobro dos elementos de 1 á 100, desde que sejam pares

f3 = [x*2 | x <- [1..100], rem x 2 == 0]
f3' = [x*2 | x <- [1..100], even 2]

--even 10 >> True
--even 11 >> False

--odd 10 >> False
--odd 11 >> True

-- Dobro dos pares entre 1 e 100 maiores que 50

f3'' = [x*2 | x <- [1..100], even x, x > 50]

-- Exercicio 7: Implementar a função tamanha (lenght) utilizando compreensão de listas

tamanho xs = sum [1 | _ <- xs]

{-
[1,2,3,4,5]
[1,1,1,1,1]
sum
-}

{-
[1] :: [Int]
[[1],[2]] :: [[Int]]
-}

-- Exercicio 8: Implementar uma função que recebe um lista de listas
-- [[]]  e retorna uma lista.

listToList [] = []
listToList (x:xs) = x ++ listToList xs


-- Exercicio 9: Implementar que recebe 2 listas, e retorne uma lista
-- com a soma do primeiro elemento da primeira lista, com o primeiro
-- elemento da segunda lista.
-- [1,2,3] [4,5,6] >> [5,7,9]

-- Versão com recursão

somaListas [] [] = []
somaListas [] _ = []
somaListas _ [] = []
somaListas (x:xs) (y:ys) = (x+y) : somaListas xs ys

somaListas' xs xy = [x + y | x <- xs, y <- ys]
