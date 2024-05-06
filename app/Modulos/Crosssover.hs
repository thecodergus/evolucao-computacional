module Crosssover where


import Tipos (Individuo(genes, Individuo), Populacao)
import Utils.Aleatoriedades (randomFloat, randomInt)
import Utils.Outros (selecionarRemover)
import Data.List (elemIndex)
import Data.Bifunctor(bimap)

crossover :: Ord a => Populacao a -> ((Individuo a, Individuo a) -> IO (Individuo a, Individuo a)) -> IO (Populacao a)
crossover [] _ = return []
crossover [a] _ = return [a]
crossover populacao estrategiaCrossover = do
  -- Escolher Pai
  (pai, populacao') <- selecionarRemover populacao

  -- Escolher mãe
  (mae, populacao'') <- selecionarRemover populacao'

  -- Iterar sobre o resto da população
  restante <- crossover populacao'' estrategiaCrossover

  -- Realizar o crossover entre o pai e a mãe
  (maisVelho, maisNovo) <- estrategiaCrossover (pai, mae)

  return $ maisVelho : maisNovo : restante

-- A função 'umPontoAleatorio' recebe dois indivíduos (pai e mãe) como entrada.
-- Ela retorna um par de novos indivíduos que são o resultado do crossover de um ponto aleatório entre o pai e a mãe.
umPontoAleatorio :: (Individuo a, Individuo a ) -> Float -> IO (Individuo a, Individuo a)
umPontoAleatorio (pai, mae) probabilidade
  -- Verifica se o tamanho dos genes do pai e da mãe são iguais. Se não forem, lança um erro.
  | length (genes pai) /= length (genes mae) = error "O tamanhos dos genes do pai e da mae devem ser iguais"
  -- Verifica se a lista de genes do pai é vazia. Se for, lança um erro.
  | null (genes pai) = error "O numero de genes devem ser maiores que zero"
  -- Se nenhuma das condições acima for verdadeira, realiza o crossover.
  | otherwise = umPontoAleatorio'
  where
    -- A função 'umPontoAleatorio' não recebe nenhum argumento.
    -- Ela retorna um par de novos indivíduos que são o resultado do crossover de um ponto aleatório entre o pai e a mãe.
    umPontoAleatorio' = do
      -- Gera um número aleatório entre 0 e 1.
      valorAleatorio <- randomFloat (0, 1)

      -- Chama a função 'mutar' passando o valor aleatório, a lista de genes do pai e a lista de genes da mãe.
      mutar (valorAleatorio <= probabilidade) (genes pai) (genes mae)

      where 
        mutar :: Bool -> [a] -> [a] -> IO (Individuo a, Individuo a)
        mutar False a b = return (Individuo a 0, Individuo b 0)
        mutar True a b = do
          -- Gera um número aleatório entre 0 e o tamanho dos genes do pai menos 1.
          numero_aleatorio <- randomInt (0, length a - 1)
          -- Divide a lista de genes do pai no ponto aleatório.
          let (pai_1, pai_2) = splitAt numero_aleatorio a
          -- Divide a lista de genes da mãe no ponto aleatório.
          let (mae_1, mae_2) = splitAt numero_aleatorio b
          -- Retorna um par de novos indivíduos. O primeiro indivíduo tem a primeira parte dos genes da mãe e a segunda parte dos genes do pai.
          -- O segundo indivíduo tem a primeira parte dos genes do pai e a segunda parte dos genes da mãe.
          return (Individuo (mae_1 ++ pai_2) 0, Individuo (pai_1 ++ mae_2) 0)



-- A função 'doisPontosAleatorios' recebe dois indivíduos (pai e mãe) como entrada.
-- Ela retorna um par de novos indivíduos que são o resultado do crossover de dois pontos aleatórios entre o pai e a mãe.
doisPontosAleatorios :: (Individuo a, Individuo a) -> Float -> IO (Individuo a, Individuo a)
doisPontosAleatorios (pai, mae) probabilidade
  | length (genes pai) /= length (genes mae) = error "O tamanhos dos genes do pai e da mae devem ser iguais"
  | null (genes pai) = error "O numero de genes devem ser maiores que zero"
  | otherwise = doisPontosAleatorios'
  where
    doisPontosAleatorios' = do
      valorAleatorio <- randomFloat (0, 1) -- Gera um valor aleatório entre 0 e 1
      
      mutar (valorAleatorio <= probabilidade) (genes pai) (genes mae)
      where
        mutar :: Bool -> [a] -> [a] -> IO (Individuo a, Individuo a)
        mutar False a b = return (Individuo a 0, Individuo b 0)
        mutar True a b = do
          -- Gera dois números aleatórios entre as posições válidas dos genes do pai.
          numero_aleatorio1 <- randomInt (0, length a - 1)
          numero_aleatorio2 <- randomInt (0, length a - (numero_aleatorio1 + 1))

          -- Divide a lista de genes do pai nos dois pontos aleatórios.
          let (pai_1, pai_2) = splitAt numero_aleatorio1 a
              (pai_3, pai_4) = splitAt (numero_aleatorio2 - numero_aleatorio1) pai_2

          -- Divide a lista de genes da mãe nos dois pontos aleatórios.
          let (mae_1, mae_2) = splitAt numero_aleatorio1 b
              (mae_3, mae_4) = splitAt (numero_aleatorio2 - numero_aleatorio1) mae_2

          -- Retorna um par de novos indivíduos. O primeiro indivíduo tem a primeira parte dos genes do pai, a parte do meio dos genes da mãe e a última parte dos genes do pai.
          -- O segundo indivíduo tem a primeira parte dos genes da mãe, a parte do meio dos genes do pai e a última parte dos genes da mãe.
          return (Individuo (pai_1 ++ mae_3 ++ pai_4) 0, Individuo (mae_1 ++ pai_3 ++ mae_4) 0)

uniforme :: (Individuo a, Individuo a) -> Float -> IO (Individuo a, Individuo a)
uniforme (Individuo lista_1 _, Individuo lista_2 _) probabilidade
  | length lista_1 /= length lista_2 = error "O tamanhos dos genes do pai_1 e da mae devem ser iguais"
  | null lista_1 = error "O numero de genes devem ser maiores que zero"
  | otherwise = do
    valorAleatorio <- randomFloat (0, 1) -- Gera um valor aleatório entre 0 e 1

    mutar (valorAleatorio <= probabilidade) lista_1 lista_2
    
    where
      mutar :: Bool -> [a] -> [a] -> IO (Individuo a, Individuo a)
      mutar False a b = return (Individuo a 0, Individuo b 0)
      mutar True a b = do
        (filho_1, filho_2) <- uniforme' a b

        return (Individuo filho_1 0, Individuo filho_2 0)

      uniforme' :: [a] -> [a] -> IO ([a], [a])
      uniforme' [] [] = return ([], [])
      uniforme' a [] = return (a, [])
      uniforme' [] b = return ([], b)
      uniforme' (x:xs) (y:ys) = do
        valorAleatorio <- randomInt (0, 1) -- Gera um valor aleatório entre 0 e 1
        (filho_1, filho_2) <- uniforme' xs ys -- Realiza o crossover uniforme nas caudas das listas

        -- Decide quais elementos serão adicionados aos filhos com base no valor aleatório
        let filho_1' = if valorAleatorio == 1 then x : filho_1 else y : filho_1
        let filho_2' = if valorAleatorio == 1 then y : filho_2 else x : filho_2

        return (filho_1', filho_2')


-- Função que realiza o crossover PMX entre dois indivíduos com probabilidade de mutação
pmx :: Eq a => (Individuo a, Individuo a) -> Float -> IO (Individuo a, Individuo a)
pmx (Individuo gene_pai _, Individuo gene_mae _) probabildiade = do
  -- Gera um número aleatório entre 0 e 1 para decidir se a mutação ocorrerá ou não
  chanceMutar <- randomFloat (0, 1)

  -- Realiza a mutação dos genes dos pais para gerar os filhos, caso a chance gerada seja menor ou igual à probabilidade
  (gene_filho_mais_velho, gene_filho_mais_novo) <- mutar (chanceMutar <= probabildiade) gene_pai gene_mae

  -- Retorna os filhos gerados
  return (Individuo gene_filho_mais_velho 0, Individuo gene_filho_mais_novo 0)
  where
    -- Função que realiza a mutação dos genes dos pais para gerar os filhos
    mutar :: Eq a => Bool -> [a] -> [a] -> IO ([a], [a])
    mutar False gene_pai' gene_mae' = return (gene_pai', gene_mae') -- Se a mutação não ocorrer, retorna os pais como filhos
    mutar True gene_pai' gene_mae' = do
      -- Gera dois pontos de corte aleatórios para a realização do crossover
      pontos_corte <- gerarNumeroAleatorio 0 (length gene_pai' - 1)

      -- Realiza o crossover PMX entre os pais e retorna os filhos gerados
      return $ mutar' pontos_corte gene_pai' gene_mae'
      where
        -- Função que realiza o crossover PMX entre os pais
        mutar' :: Eq a => (Int, Int) -> [a] -> [a] -> ([a], [a])
        mutar' pontos pai mae = do
          -- Corta as listas dos pais em três partes cada uma com base nos pontos de corte
          let (pai_parte_1, pai_matching_section, pai_parte_2) = cortarLista pontos pai
          let (mae_parte_1, mae_matching_section, mae_parte_2) = cortarLista pontos mae

          -- Realiza o matching das seções dos pais e gera os filhos
          (fazerMatching (pai_parte_1, mae_matching_section, pai_parte_2) pai_matching_section, fazerMatching (mae_parte_1, pai_matching_section, mae_parte_2) mae_matching_section)
          where
            -- Função que corta uma lista em três partes com base nos pontos de corte
            cortarLista :: (Int, Int) -> [a] -> ([a], [a], [a])
            cortarLista (a, b) lista = do
              let (parte_1, resto) = splitAt a lista
                  (parte_2, parte_3) = splitAt (b - a) resto

              (parte_1, parte_2, parte_3)

            -- Função que realiza o matching das seções dos pais para gerar os filhos
            fazerMatching :: Eq a => ([a], [a], [a]) -> [a] -> [a]
            fazerMatching (parte_1, match, parte_2) matchSection = fazerMatching' parte_1 matchSection ++ match ++ fazerMatching' parte_2 matchSection
              where
                -- Função auxiliar para realizar o matching das seções dos pais
                fazerMatching' :: Eq a => [a] -> [a] -> [a]
                fazerMatching' [] _ = []
                fazerMatching' (a : as) matchSection'
                  | Just i <- elemIndex a matchSection' = [matchSection' !! i]
                  | otherwise = a : fazerMatching' as matchSection'

    -- Função que gera dois números aleatórios diferentes entre si para os pontos de corte
    gerarNumeroAleatorio :: Int -> Int -> IO (Int, Int)
    gerarNumeroAleatorio a b
      | a == b = error "Ta de sacanagem mane, dois numeros iguais?"
      | otherwise = gerarNumeroAleatorio' a b
      where
        gerarNumeroAleatorio' :: Int -> Int -> IO (Int, Int)
        gerarNumeroAleatorio' a' b' = do
          a'' <- randomInt (a', b')
          b'' <- randomInt (a', b')

          if a'' == b''
            then gerarNumeroAleatorio' a b
            else return $ retornarMaior a'' b''

        -- Função auxiliar que retorna os dois números gerados em ordem decrescente
        retornarMaior :: Int -> Int -> (Int, Int)
        retornarMaior a' b'
          | a' > b' = (b', a')
          | otherwise = (a', b')


-- cx é uma função que recebe dois indivíduos (pai e mãe) e uma probabilidade, e realiza o cruzamento entre eles
cx :: Eq a => (Individuo a, Individuo a) -> Float -> IO (Individuo a, Individuo a)
cx (pai, mae) probabilidade = do
  chanceMutar <- randomFloat (0, 1) -- gera um número aleatório entre 0 e 1 para decidir se o cruzamento ocorrerá ou não

  cx' pai mae (chanceMutar <= probabilidade)
  where
    -- cx' é uma função auxiliar que realiza o cruzamento em si, caso a chance de mutar seja menor ou igual à probabilidade
    cx' :: Eq a => Individuo a -> Individuo a -> Bool -> IO (Individuo a, Individuo a)
    cx' pai' mae' False = return (pai', mae') -- se não houver cruzamento, retorna os indivíduos originais
    cx' (Individuo [] _) (Individuo [] _) _ = return (Individuo [] 0, Individuo [] 0) -- se ambos os indivíduos estiverem vazios, retorna dois indivíduos vazios
    cx' (Individuo [] _) (Individuo mae' _) True = return (Individuo mae' 0, Individuo mae' 0) -- se o pai estiver vazio, retorna dois indivíduos idênticos à mãe
    cx' (Individuo pai' _) (Individuo [] _) True = return (Individuo pai' 0, Individuo pai' 0) -- se a mãe estiver vazia, retorna dois indivíduos idênticos ao pai
    cx' (Individuo (p:ps) _) (Individuo (m:ms) _) True = do
      let comparacoes = compararListas ps ms -- compara as listas de genes dos indivíduos para encontrar os genes comuns

      let (filho_mais_velho, filho_mais_novo) = trocarPosicao ps ms comparacoes 0 -- troca a posição dos genes comuns nos indivíduos

      return (Individuo  (p : filho_mais_velho) 0, Individuo (m : filho_mais_novo) 0) -- retorna os dois novos indivíduos resultantes do cruzamento

      where
        -- função auxiliar que compara as listas de genes e retorna as posições dos genes comuns
        compararListas :: (Eq a) => [a] -> [a] -> [Int]
        compararListas xs ys = procurarCompartibilidade  [(i, j) | (i, x) <- zip [0 ..] xs, (j, y) <- zip [0 ..] ys, x == y]
          where
            -- função auxiliar que procura as posições dos genes comuns
            procurarCompartibilidade :: [(Int, Int)] -> [Int]
            procurarCompartibilidade [] = [] -- se não houver genes comuns, retorna uma lista vazia
            procurarCompartibilidade ((a, b) : xs')
              | a == b = a : procurarCompartibilidade xs' -- se as posições forem iguais, adiciona a posição à lista de resultados
              | a `elem` map snd xs' = a : procurarCompartibilidade xs' -- se a primeira posição já estiver na lista de resultados, adiciona a segunda posição
              | b `elem` map fst xs' = b : procurarCompartibilidade xs' -- se a segunda posição já estiver na lista de resultados, adiciona a primeira posição
              | otherwise = procurarCompartibilidade xs' -- senão, continua procurando

        -- função auxiliar que troca as posições dos genes comuns nos indivíduos
        trocarPosicao :: [a] -> [a] -> [Int] -> Int -> ([a], [a])
        trocarPosicao [] _ _ _ = ([], []) -- se uma das listas estiver vazia, retorna duas listas vazias
        trocarPosicao _ [] _ _ = ([], [])
        trocarPosicao _ _ [] _ = ([], [])
        trocarPosicao (p':ps') (m':ms') posicoes i
          | i `elem` posicoes = bimap (p' :) (m' :) (trocarPosicao ps' ms' posicoes (i + 1)) -- se a posição atual for uma posição de gene comum, troca os genes nos indivíduos
          | otherwise = bimap (m' :) (p' :) (trocarPosicao ps' ms' posicoes (i + 1)) -- senão, mantém os genes nos indivíduos originais
