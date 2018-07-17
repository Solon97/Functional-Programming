{- Questão 1 - Quadrado do dobro -}
quadrado_dobro :: Int -> Int
quadrado_dobro n = (n*2)^2

{- Questão 2 - Valor absoluto -}
valor_absoluto :: Int -> Int
valor_absoluto n = 
    if n < 0
        then n*(-1)
        else n*1

{- Questão 3 - Abono Salarial -}
calculo_abono_salarial :: Int -> Float
calculo_abono_salarial n
        | n > 0 && n < 11 = 100
        | n > 10 && n < 21 = 200
        | n > 20 && n < 31 = 300
        | n > 30 && n < 41 = 400
        | n > 40 = 500 

{- Questão 4 - Se o número é par-}
numero_eh_par :: Int -> Bool
numero_eh_par n = 
    if (mod n 2 /= 0)
        then False
        else True

{- Questão 5 - Calculo da Força de compressão 
compressao :: Float -> Float -> Float
compressao k f = 
-}

{- Questão 6 - Calculo de Kilowatts-}
calculo_quilowatt_salario :: Float -> Float -> Float
calculo_quilowatt_salario s q = 0.85 * q * (s/5)

{- Questão 7 - Calculo da Potência -}
potencia :: Int -> Int -> Int
potencia base 0 = 1
potencia 0 exp = 0
potencia base exp = potencia(base (exp-1)) * base