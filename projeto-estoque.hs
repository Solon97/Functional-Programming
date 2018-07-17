{-
  Sistema de Controle de Estoque
  Produtos - Adicionar, Filtrar e Listar produtos
-}
  import System.Process

-- Declaração de Tipos
  type Produtos = [Produto]
  type Codigo = Int
  type Nome = String
  type Endereco = String
  type Preco = Double
  type Qtd = Int

  data Produto = Produto Codigo Nome Preco Qtd
-- Inicialização do Sistemas
  main :: IO Produtos
  main = menu []

  menu :: Produtos -> IO Produtos
  menu bd_produtos = do 
                system "cls"
                putStrLn "----------- Controle de Estoque -----------"
                putStrLn "1- Cadastrar produto"
                putStrLn "2- Editar produto"
                putStrLn "3- Listar Produtos"
                putStrLn "4- Excluir Produto"
                putStrLn "0- Sair"
                putStr "Opção: "
                opcao <- readLn
                controle_menu bd_produtos opcao
  controle_menu :: Produtos -> Int -> IO Produtos
  controle_menu bd_produtos opcao | (opcao == 1) = do system "cls"
                                                      adicionar bd_produtos 
                                  | (opcao == 2) = do system "cls"
                                                      editar bd_produtos
                                  | (opcao == 3) = do system "cls"
                                                      putStrLn "----------- Controle de Estoque -----------"
                                                      putStrLn "------------- Listar Produtos -------------"
                                                      controle_listar bd_produtos bd_produtos
                                  | (opcao == 4) = do system "cls"
                                                      excluir bd_produtos
                                  | (opcao == 0) = do system "cls"
                                                      putStrLn ("Volte Sempre!")
                                                      return bd_produtos
                                  | otherwise = do putStrLn ("Opção Inválida")
                                                   getChar
                                                   menu bd_produtos
-- Adicionar
  adicionar :: Produtos -> IO Produtos
  adicionar bd_produtos = do 
                      putStrLn "----------- Controle de Estoque -----------"
                      putStrLn "----------- Adicionar Produto -------------"
                      putStr "Código: "
                      codigo <- readLn
                      putStr "Nome: "
                      nome <- readLn
                      putStr "Preço: "
                      preco <- readLn
                      putStr "Quantidade: "
                      qtd <- readLn
                      let bd_atualizado = salvar bd_produtos (Produto codigo nome preco qtd)  
                      putStrLn "------ Produto Adicionado! ------"
                      getChar
                      menu bd_atualizado     
                                    
  salvar :: Produtos -> Produto -> Produtos
  salvar bd_produtos produto  =  bd_atualizado
                                  where
                                    bd_atualizado = produto:bd_produtos

--Editar
  editar :: Produtos -> IO Produtos
  editar bd_produtos = do putStrLn "----------- Controle de Estoque -----------"
                          putStrLn "----------- Edição de Produtos -------------"
                          putStr "Informe o código do produto desejado: "
                          codigo <- readLn
                          putStr "Informe a nova quantidade do produto: "
                          qtd <- readLn
                          let produto = filtrar_produto bd_produtos codigo
                          let produto_alterado = alterar_qtd_produtos produto qtd
                          let bd_produtos_atualizado = atualizar_bd bd_produtos [] produto_alterado
                          --Detalhamento do Produto Alterado
                          putStrLn "Produto Alterado com Sucesso."
                          getChar
                          menu bd_produtos_atualizado

  alterar_qtd_produtos :: Produto -> Int -> Produto
  alterar_qtd_produtos produto qtd_alterada = produto_alterado
                                                where  
                                                  codigo = get_cod produto
                                                  nome = get_nome produto
                                                  preco = get_preco produto
                                                  qtd = get_qtd produto + qtd_alterada
                                                  produto_alterado = (Produto codigo nome preco qtd_alterada)
  
  atualizar_bd :: Produtos -> Produtos -> Produto -> Produtos
  atualizar_bd [] bd_atualizado produto_alterado = bd_atualizado
  atualizar_bd (x:xs) bd_atualizado produto_alterado | ((get_cod x) == (get_cod produto_alterado)) = atualizar_bd xs bd_produto_alterado produto_alterado
                                                     | otherwise = atualizar_bd xs bd_produto_mantido produto_alterado
                                                          where
                                                            bd_produto_alterado = produto_alterado:bd_atualizado
                                                            bd_produto_mantido = x:bd_atualizado
  filtrar_produto :: Produtos -> Int -> Produto
  filtrar_produto (x:xs) codigo | ((get_cod x) == codigo) = x
                                | otherwise = filtrar_produto xs codigo
-- Listar
  listar :: Produtos -> Produtos -> Produto -> IO Produtos
  listar bd_produtos produtos_listar produto = do 
                                                      putStrLn "-------------------------------------------"
                                                      putStr "Codigo: "
                                                      putStrLn (show (get_cod produto))
                                                      putStr "Nome: "
                                                      putStrLn (show (get_nome produto))
                                                      putStr "Preço: "
                                                      putStrLn (show (get_preco produto))
                                                      putStr "Quantidade: "
                                                      putStrLn (show (get_qtd produto))
                                                      controle_listar bd_produtos produtos_listar
  controle_listar :: Produtos -> Produtos -> IO Produtos
  controle_listar [] [] = do 
                            putStrLn "-------- Nenhum Produto no Estoque --------"    
                            getChar
                            menu []
  controle_listar bd_produtos [] = do 
                                      putStrLn "-------------- Fim da lista ---------------"
                                      getChar
                                      menu bd_produtos
  controle_listar bd_produtos (x:xs) = listar bd_produtos xs x
-- Excluir
  excluir :: Produtos -> IO Produtos
  excluir bd_produtos = do 
                          putStrLn "----------- Controle de Estoque -----------"
                          putStrLn "----------- Exclusão de Produtos -------------"
                          putStr "Informe o código do produto desejado: "
                          codigo <- readLn
                          let bd_produtos_atualizado = filter (remover_produto codigo) bd_produtos 
                          putStrLn "Produto Excluído com Sucesso!"
                          getChar
                          controle_listar  bd_produtos_atualizado bd_produtos_atualizado
                          menu bd_produtos

  remover_produto :: Int -> Produto -> Bool
  remover_produto codigo produto = (get_cod produto) /= codigo

-- Selecionando atributos do produto para Listagem
  get_cod :: Produto -> Int
  get_cod (Produto x _ _ _) = x
  get_nome :: Produto -> String
  get_nome (Produto _ x _ _) = x
  get_preco :: Produto -> Double
  get_preco (Produto _ _ x _) = x
  get_qtd :: Produto -> Int
  get_qtd (Produto _ _ _ x) = x