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
-- Exemplo? inverso[1,2,3,4] =>[4,3,2,1]
