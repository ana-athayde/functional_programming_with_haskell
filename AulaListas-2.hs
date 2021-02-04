altura_familia = [1.72, 1.68, 1.71, 1.89]
    -- 1.73 : 1.68 : 1.71 : 1.89 : []


acessar_altura_fam [] _ = error("Não existem registros!")
acessar_altura_fam@(x:xs) pos_fam | pos_fam > 0 = acessar_altura_fam xs (pos_fam-1)
    | otherwise = x

--

casa_tipo_comodos = ["sala", "cozinha", "quarto", "banheiro", "corredor"]

casa_inf_area_comodos = [ [3,5], [2,4], [2.5,2], [2,2.4] ]

--head (x:xs) = x

acessar_altura_comodo :: [[Double]] -> Double -> Double
acessar_altura_comodo [] _ = error("Não existem registros!")
acessar_altura_comodo@(x:xs) pos_comodo = if pos_comodo > 1 then acessar_altura_comodo xs (pos_comodo - 1 )
    else head x

acessar_alturas_casa :: [[Double]] -> [Double]
acessar_alturas_casa [] = []
acessar_alturas_casa casa@(x:xs) = head x : acessar_alturas_casa xs

alterar_altura_comodo :: [[Double]] -> Int -> Double -> [[Double]]
alterar_altura_comodo [] _ _ = []
alterar_altura_comodo casa@(x:xs) pos_comodo a | pos_comodo > 1 = x : (alterar_altura_comodo xs (pos_comodo - 1) a )
    | otherwise = [a,last x] : xs

calcular_casa_m2_comodos :: [[Double]] -> [Double]
calcular_casa_m2_comodos [] = []
calcular_casa_m2_comodos[] casa@(x:xs) = altura * largura : calcular_casa_m2_comodos xs
    where altura = head x
         largura = last x

-- [[nmro de janelas, altura, largura]]
-- acessar, alterar, calcular (exercicio para proxima aula)
-- asa_inf_area_comodos = [ [2,3,5], [1,2,4], [1,2,2.4], [0,1.2,2] ]

mostrar_nome_dimensoes_comodos :: [[Char]] -> [[Double]] -> [[Char]]
mostrar_nome_dimensoes_comodos [] [] = []
mostrar_nome_dimensoes_comodos [] _ = []
mostrar_nome_dimensoes_comodos _ [] = [] 
mostrar_nome_dimensoes_comodos tipo@(x:xs) al@(y:ys) = msg : mostrar_nome_dimensoes_comodos xs ys
    where msg = x ++ " tem " ++ show(head y) ++ " metros de altura e " ++ show(last y) ++ "metros de largura."

--"sala tem 3 metros de altura e 5 metros de largura", 
--"cozinha",
--"banheiro",
--"corredor"

--casa_tipo_comodos = ["sala", "cozinha", "quarto", "banheiro", "corredor"]
--casa_inf_area_comodos = [ [3,5], [2,4], [2.5,2], [2,2.4], [1.2,2] ]
