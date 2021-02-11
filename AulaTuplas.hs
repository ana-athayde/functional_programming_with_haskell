{-
- Também é uma forma de armazenar uma coleção.
- Lista tem tamanho flexível mas tipo rígido.
- Tuplas tem tamanho rígido e tipo flexível.

- Syntax: () ,""
-}

casa_tipo_comodos = ["sala","cozinha", "quarto","banheiro", "corredor"]
casa_inf_area_comodos = [[3, 5], [2, 4], [2.5, 2], [2, 2.4], [1.2, 2]]
-- Problemas:
-- 1. Não existe uma ligação direta do cômodo com as suas dimensões.
-- 2. Não existe nada que limite o tamanho das listas.

dimensão_comodo = (3, 5)
-- comodo_casa = ("casa", 3, 5)
comodo_casa = ("sala", 3, 5) -- comodo_casa = ("sala", (3,5))
comodos_casa = [("casa", 3, 5), ("cozinha", 2, 4), ("quarto", 2.5, 2), ("banheiro", 2, 2.4), ("corredor", 1.2, 2)]

t1 = fst dimensão_comodo
t2 = snd dimensão_comodo

-- fst e snd só funcionam para tuplas de tamanho 2

-- usando casamento de padrões, http://www.decom.ufop.br/romildo/2014-1/bcc222/practices/p07-padroes.pdf
-- http://www.decom.ufop.br/romildo/2018-1/bcc222/slides/progfunc.pdf na página 127 do pdf fala sobre casamento de padrões

fst' (x, y) = x
snd' (x, y) = y

fst3 (x, y, z) = x
snd3 (x, y, z) = y
trd3 (x, y, z) = z

t4 = fst3 comodo_casa

retorna_altura_banheiro (x:y:z:w:xs) = snd3 w

{-
acessar_altura_comodo [] _ = error("Cômodo não existente!")
acessar_altura_comodo@(x:xs) pos_comodo = if pos_comodo > 1 then acessar_altura_comodo xs (pos_comodo - 1 )
    else head x
-}

acessar_altura_comodo [] _ = error("Cômodo não existente!")
acessar_altura_comodo (x:xs) comodo | fst3 x == comodo = snd3 xs
    otherwise = acessar_altura_comodo xs comodo

{-
acessar_alturas_casa [] = []
acessar_alturas_casa casa@(x:xs) = head x : acessar_alturas_casa xs
-}

acessar_alturas_casa [] = []
acessar_alturas_casa (x:xs) = snd3 x : acessar_alturas_casa xs

-- x = ("sala", 3, 5)
-- xs = [("cozinha", 2, 4), ("quarto", 2.5, 2), ("banheiro", 2, 2.4), ("corredor", 1.2, 2)]

{-
alterar_altura_comodo [] _ _ = error("Cômodo não existente")
alterar_altura_comodo casa@(x:xs) pos_comodo a | pos_comodo > 1 = x : (alterar_altura_comodo xs (pos_comodo - 1) a )
    | otherwise = [a,last x] : xs
-}

alterar_altura_comodo [] _ _ = error("Cômodo não existente")
alterar_altura_comodo (x:xs) comodo nova_altura = if (fst3 x) == comodo then (fst3 x, nova_altura, trd3 x)
    else x : (alterar_altura_comodo xs comodo nova_altura)

calcular_casa_m2_comodos [] = []
calcular_casa_m2_comodos (x:xs) = snd x * trd3 x : calcular xs

mostrar_nome_dimensoes_comodos [] = []
mostrar_nome_dimensoes_comodos (x:xs) = msg : mostrar_nome_dimensoes_comodos xs
    where msg = fst3 x ++ "tem" ++ show(snd3 x) ++ "metros de altura e " ++ show(trd3 x) ++ "metros de largura"

-- Exercicio 1
-- Considere uma coleção para armazenar os hóspedes (nome e ano) de cada um dos quartos de um
-- hotel, como você definiria essa estrutura?

hospedes = [("Quarto 1", [("Ana". 2008), ("João", 2009)]), ("Quarto 2", [("Maria", 2011)])]

-- Exercicio 1
-- Refazer as funções acima considerando a lista abaixo:
comodos_casa_com_janelas = [("sala", 2, (3, 5)), ("cozinha", 3, (2, 4)), ("quarto", 1, (2.5, 2)), ("banheiro", 1, (2, 2.4)), ("corredor", 0, (1.2, 2))]

-- ("sala", 2, (3, 5)) - "sala" - nome do cômodo | 2 - número de janelas | 3,5 dimensões 
