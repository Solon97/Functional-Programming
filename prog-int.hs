--Questão 1
  celsius :: Double -> Double
  celsius temp_fahr = 5/9 * (temp_fahr - 32)

  main_1 :: IO()
  main_1 = do putStrLn "Digite uma temperatura na escala Fahrenheit: "
              temp_fahr <- readLn :: IO Double
              putStr "Temperatura em Farenheit: "
              putStrLn (show temp_fahr)
              putStr "Temperatura em Celsius: "            
              putStrLn (show (celsius temp_fahr))  

-- Questão 2
  analise_credito :: Double -> Double -> String
  analise_credito salario prestacao | (prestacao <= (salario*0.3)) = "O empréstimo pode ser concedido"
                                    | otherwise = "O empréstimo não pode ser concedido"
  main_2 :: IO()
  main_2 = do putStrLn "Análise de crédito"
              putStrLn "---------------------------------"
              putStr "Salário buto: "
              salario <- readLn :: IO Double
              putStr "Valor da prestação: "
              prestacao <- readLn
              putStrLn (analise_credito salario prestacao)

-- Questão 3
  idade_eleitor :: Int -> String
  idade_eleitor idade | (idade < 16) = "Não Eleitor"
                      | (idade >= 18 && idade < 65) = "Eleitor Obrigatório"
                      | (idade >= 16 && idade < 18 || idade >= 65) = "Eleitor Facultativo"

  main_3 :: IO()
  main_3 = do putStrLn "Classe Eleitoral"
              putStrLn "---------------------------------"
              putStr "Digite a idade da pessoa: "
              idade <- readLn
              putStrLn (idade_eleitor idade)

-- Questão 4
  calculo_imposto :: Double -> Double
  calculo_imposto salario | (salario < 500.0) = salario*0.05
                          | (salario >= 500.0 && salario <= 850.0) = salario*0.1
                          | (salario > 850.0) = salario*0.15
  imposto :: IO()
  imposto = do putStrLn "Cálculo do imposto"
               putStr "Digite o salário: "
               salario <- readLn :: IO Double
               putStr "Imposto calculado: "
               putStrLn (show (calculo_imposto salario))

  calculo_novo_salario :: Double -> Double
  calculo_novo_salario salario | (salario > 1500.0) = salario + 25.0
                               | (salario >= 750.0 && salario <= 1500.0) = salario + 50.0
                               | (salario >= 450.0 && salario < 750.0) = salario + 75.0
                               | (salario < 450.0) = salario + 100.0
  novo_salario :: IO()
  novo_salario = do putStrLn "Cálculo do novo salário"
                    putStr "Digite o salário: "
                    salario <- readLn :: IO Double
                    putStr "Novo salário: "
                    putStrLn (show (calculo_novo_salario salario))
  
  calculo_classificacao :: Double -> String   
  calculo_classificacao salario | (salario <= 1500.0) = "Mal remunerado"
                                | (salario > 1500.0) = "Bem remunerado"

  classificacao :: IO()
  classificacao = do putStrLn "Classificação do salário"
                     putStr "Digite o salário: "
                     salario <- readLn :: IO Double
                     putStr "Classificação obtida: "
                     putStrLn (calculo_classificacao salario)
  invalido :: IO()
  invalido = do putStrLn "Opção inválida"

  menu :: Int -> IO()
  menu opcao | (opcao == 1) = imposto
             | (opcao == 2) = novo_salario
             | (opcao == 3) = classificacao
             | otherwise = invalido

  main_4 :: IO()
  main_4 = do putStrLn "------------------"
              putStrLn "Opções"
              putStrLn "------------------"
              putStrLn "1. Imposto"
              putStrLn "2. Novo salário"
              putStrLn "3. Classificação"
              putStrLn "------------------"
              putStrLn "Digite a opção desejada: "
              opcao <- readLn
              menu opcao