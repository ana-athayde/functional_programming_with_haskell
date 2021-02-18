import Data.Char

-- Declare uma função que recebe uma lista e retorna uma tupla, com o menor na esquerda
-- e o maior à direita.
--menorMaior :: [Int] -> (Int)

menorMaior xs = (menor xs, maior xs)

menor [x] = x
menor (x:xs) = if x < menor xs then x else menor xs
    where r = menor xs 

maior [x] = x
maior (x:xs) = if x < maior xs then x else maior xs
    where r = maior xs

-- Declare uma função que recebe uma lista e um número n, e retorne uma tupla com os
-- menores que n à esquerda e os maiores iguas que n à direita

-- menoresMaiores :: [Int] - > Int 
-- menoresMaiores :: Ord a => [a] -> a -> ([a],[a])

menoresMaiores xs n = (menores xs n, maiores xs n)

menores [] _ = []
menores (x:xs) n = if x < n then x:r else r
    where r menores xs n

maiores [] _ = []
maiores (x:xs) n = if x >= n then x:r else r
    where r maiores xs n

-- []
-- :
-- [1,2,3,4,5] >> 1:2:3:4:5:[]

-- Recursão e casamento de padrões, compreensão de listas

-- [1,2,3,4,5] >> [2,4,6,8,10]
dobro [] = []
dobro (x:xs) = 2*x : dobro xs
-- dobro (x:xs) = if x >= 5 then 2*x : dobro xs else dobro xs

dobro' xs = [2*x | x <- xs]
-- dobro' xs = [2*x | x <- xs, x >= 5]

-- menores' xs n = [a | a <- xs, a < n]
-- maiores' xs n = [b | b <- xs, b >= n]

menoresMaiores' xs n ( [a | a <- xs, a < n], [b | b <- xs, b >= n] )
[4:13 PM] ADELAINE FRANCIELE GELAIN
    
{- Declare uma função que receba como parâmetro uma String e retorne uma dupla de 
Strings, a primeira String deve conter as letras maiúsculas e a segunda as letras 
minúsculas. Os caracteres que não forem letras devem ser ignorados (Olhar as funções 
isLower e isUpper do módulo Data.Char) 
Exemplo: maiuscMinusc “Programacao Funcional - 1” => (“PF”, “rogramacaouncional”)-}
​​​​
-- https://hackage.haskell.org/package/base-4.14.1.0/docs/Data-Char.html

minusc [] = []
-- minusc (x:xs) = if isLower x then x: minusc xs else minusc xs
minusc (x:xs) = if isLower x then x: result else result
    where result = minusc xs

maiusc [] = []
maiusc (x:xs) = if isUpper x then x: result else result
    where result = maiusc xs

minuscMaiusc xs = (minusc xs, maiusc xs)

minuscMaiusc' xs = ( [a | a <- xs, isLower a], [b | b <- xs, isUpper b] )

-- zip
zip' [] [] = []
zip' [] _ = []
zip' _ [] = []
zip' (x:xs) (y:ys) = (x,y) : zip' xs ys

zip'' xs ys = (x,y | x <- xs, y <- ys)

-- Ex1: fizzBuzz :: Int -> [String]
-- fizzBuzz
-- 10 
-- divisível por 3 - "Fizz"
-- divisível por 5 - "Buzz"
-- divisível por 3 e por 5 - "FizzBuzz"
-- n
-- ["1","2","Fizz","4","Buzz","Fizz","7","8","Fizz","Buzz"]

-- Ex2: fizzBuzz' :: Int -> [(Int, String)]
-- fizzBuzz'
-- [(1,"1"),(2,"2"),(3,"Fizz"),(4,"4"),(5,"Buzz"),(6,"Fizz"),(7,"7"),(8,"8"),(9,"Fizz"),(10,"Buzz")]
-- interpretar com zip
