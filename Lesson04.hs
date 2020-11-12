-- max 5 6 >> 6
-- succ 9 >> 10

-- Determine se um ano é bissexto dado que todo ano bissexto 
-- ou é múltiplo de 400 ou é múltiplo de 4 mas não de 100. 
-- Verifique a ordem de precedência dos operadores e a necessidade 
-- do uso de parênteses.

-- rem para saber o resto da divisão
-- rem 100 19 = 0
-- 100 `rem` 10 = 0

bissexto ano = (ano `rem` 400 == 0) || ((ano `rem` 4 == 0) && (ano `rem` 100 /= 0))

-- bissexto 2000 = (2)

soma a b = a + b
soma2 = soma 1 2
soma2' = 1 `soma` 2
soma'' x = soma 2 x

------------------- x --------------------

dobro x = x+x
dobro' x = 2*x
dobro2 x y = 2 * x + 2 * y
dobro2' x y = dobro' x + dobro' y

-- Escrevam uma função que retorne o dobro dos
-- números maiores que 100, e o próprio número caso contrário.

dobroMaiores x = if x > 100 then dobro x else x

dobroMaiores' x | x > 100 = dobro x 
				 | otherwise = x


------------------- x --------------------

maior a b = if a > b then a else b

maior3 a b c = if a > b then (if a > c then a else c)
			   else (if b > c then b else c)
			   
maior3' x y z = maior (maior x y) z

------------------- x --------------------

-- somaDigitos: escreva uma função que receba um número natural 0 a 999 e retorne
-- a soma de seus digitos.
-- Ex: somaDigitos 328 => 13

somaDigitos n = n div 100

1 >> "Um!"
2 >> "Dois!"
3 >> "Três!"
...
>> "Outro número!"

qualNro n = if n == 1 then "Um!"
			else if n == 2 then "Dois!"
			else if n == 3 then "Três!"
			else if n == 4 then "Quatro!"
			else if n == 5 then "Cinco!"
			else "Maior que 5!"

qualNro' 1 = "Um!"
qualNro' 2 = "Dois!"
qualNro' 3 = "Três!"
qualNro' 4 = "Quatro!"
qualNro' 5 = "Cinco!"
qualNro' _ = "Maior que 5!"

negacao b = if b then False else True
negacao' True = False
negacao' False = True
