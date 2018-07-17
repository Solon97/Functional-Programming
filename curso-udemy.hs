-- Função para cálculo do Fatorial de um número
  fatorial :: Integer -> Integer
  fatorial 0 = 1
  fatorial n = fatorial(n-1)*n

-- Função na qual se tem como entrada a posição do termo na sequência de fibonacci 
  -- e retorna qual termo está naquela posição
  fibonacci :: Integer -> Integer
  fibonacci 0 = 0
  fibonacci 1 = 1
  fibonacci n = fibonacci(n-1) + fibonacci(n-2)

-- Guardas
  guarda :: Integer -> Integer
  guarda x | (x == 0) = 0
           | (x == 1) = 1
           | otherwise = 10

-- Variáveis Anônimas
  -- Quando não nos interessa dar nome a uma variável, 
    -- podemos usar "_" que representa uma variável anónima nova
  my_and :: Bool -> Bool -> Bool
  my_and False _ = False
  my_and _ False = False
  my_and True True = True

-- Tuplas
  calc_tupla :: (Int, Int) -> (Int, Int) -> (Int, Int)
  calc_tupla (a,b) (c,d) = (a+c,b+d)  
  
  