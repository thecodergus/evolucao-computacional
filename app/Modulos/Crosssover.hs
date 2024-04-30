module Crosssover where


import Tipos (Individuo(genes, Individuo), Populacao)
import Utils.Aleatoriedades (randomFloat, randomInt)
import Data.List (elemIndex)


-- A função 'umPontoAleatorio' recebe dois indivíduos (pai e mãe) como entrada.
-- Ela retorna um par de novos indivíduos que são o resultado do crossover de um ponto aleatório entre o pai e a mãe.
umPontoAleatorio :: Individuo a -> Individuo a -> IO (Individuo a, Individuo a)
umPontoAleatorio pai mae 
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
            -- Gera um número aleatório entre 0 e o tamanho dos genes do pai menos 1.
            numero_aleatorio <- randomInt (0, length (genes pai) - 1)
            -- Divide a lista de genes do pai no ponto aleatório.
            let (pai_1, pai_2) = splitAt numero_aleatorio (genes pai)
            -- Divide a lista de genes da mãe no ponto aleatório.
            let (mae_1, mae_2) = splitAt numero_aleatorio (genes mae)
            -- Retorna um par de novos indivíduos. O primeiro indivíduo tem a primeira parte dos genes da mãe e a segunda parte dos genes do pai.
            -- O segundo indivíduo tem a primeira parte dos genes do pai e a segunda parte dos genes da mãe.
            return (Individuo (mae_1 ++ pai_2) 0, Individuo (pai_1 ++ mae_2) 0)

crossover :: Ord a => Populacao a -> (Individuo a -> Individuo a -> IO (Individuo a, Individuo a)) -> Float -> IO (Populacao a)
crossover [] _ _= return []
crossover [a] _ _= return [a]
crossover populacao estrategiaCrossover probabilidade  = do
  -- Escolher um valor aleaorio para avaliar se fazemos crossover
  valorAleatorio <- randomFloat (0, 1)
  
  -- Escolher Pai
  individuoPosicao <- randomInt (0, length populacao - 1)
  let pai = populacao !! individuoPosicao

  -- Removendo Pai da Populacao
  let populacao2 = filter (/= pai) populacao

  -- Escolher mãe
  individuoPosicao2 <- randomInt (0, length populacao2 - 1)
  let mae = populacao !! individuoPosicao2

  -- Removendo Mãe da População
  let populacao3 = filter (/= mae) populacao
  
  restante <- crossover populacao3 estrategiaCrossover probabilidade

  if valorAleatorio <= probabilidade then do
    -- Realiza o crossover entre o pai e a mãe
    (maisVelho, maisNovo) <- estrategiaCrossover pai mae


    return $ maisVelho : maisNovo : restante
  else
    return $ pai : mae : restante


-- A função 'doisPontosAleatorios' recebe dois indivíduos (pai e mãe) como entrada.
-- Ela retorna um par de novos indivíduos que são o resultado do crossover de dois pontos aleatórios entre o pai e a mãe.
doisPontosAleatorios :: Individuo a -> Individuo a -> IO (Individuo a, Individuo a)
doisPontosAleatorios pai mae
  | length (genes pai) /= length (genes mae) = error "O tamanhos dos genes do pai e da mae devem ser iguais"
  | null (genes pai) = error "O numero de genes devem ser maiores que zero"
  | otherwise = doisPontosAleatorios'
  where
    doisPontosAleatorios' = do
      -- Gera dois números aleatórios entre as posições válidas dos genes do pai.
      numero_aleatorio1 <- randomInt (0, length (genes pai) - 1)
      numero_aleatorio2 <- randomInt (0, length (genes pai) - (numero_aleatorio1 + 1))

      -- Divide a lista de genes do pai nos dois pontos aleatórios.
      let (pai_1, pai_2) = splitAt numero_aleatorio1 (genes pai)
          (pai_3, pai_4) = splitAt (numero_aleatorio2 - numero_aleatorio1) pai_2

      -- Divide a lista de genes da mãe nos dois pontos aleatórios.
      let (mae_1, mae_2) = splitAt numero_aleatorio1 (genes mae)
          (mae_3, mae_4) = splitAt (numero_aleatorio2 - numero_aleatorio1) mae_2

      -- Retorna um par de novos indivíduos. O primeiro indivíduo tem a primeira parte dos genes do pai, a parte do meio dos genes da mãe e a última parte dos genes do pai.
      -- O segundo indivíduo tem a primeira parte dos genes da mãe, a parte do meio dos genes do pai e a última parte dos genes da mãe.
      return (Individuo (pai_1 ++ mae_3 ++ pai_4) 0, Individuo (mae_1 ++ pai_3 ++ mae_4) 0)

uniforme :: Individuo a -> Individuo a -> IO (Individuo a, Individuo a)
uniforme (Individuo lista_1 _) (Individuo lista_2 _)  
  | length lista_1 /= length lista_2 = error "O tamanhos dos genes do pai_1 e da mae devem ser iguais"
  | null lista_1 = error "O numero de genes devem ser maiores que zero"
  | otherwise = do
    (filho_1, filho_2) <- uniforme' lista_1 lista_2

    return (Individuo filho_1 0, Individuo filho_2 0)
    where
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


pmx :: Eq a => Individuo a -> Individuo a -> Float -> IO (Individuo a, Individuo a)
pmx (Individuo gene_pai _) (Individuo gene_mae _) probabildiade = do
  chanceMutar <- randomFloat (0, 1)

  (gene_filho_mais_velho, gene_filho_mais_novo) <- mutar (chanceMutar <= probabildiade) gene_pai gene_mae

  return (Individuo gene_filho_mais_velho 0, Individuo gene_filho_mais_novo 0)

  where
    mutar :: Eq a => Bool -> [a] -> [a] -> IO ([a], [a])
    mutar False gene_pai' gene_mae' = return (gene_pai', gene_mae')
    mutar True gene_pai' gene_mae' = do
      pontos_corte <- gerarNumeroAleatorio 0 (length gene_pai' - 1)

      return $ mutar' pontos_corte gene_pai' gene_mae'

      where
        mutar' :: Eq a => (Int, Int) -> [a] -> [a] -> ([a], [a])
        mutar' pontos pai mae = do
          let (pai_parte_1, pai_matching_section, pai_parte_2) = cortarLista pontos pai
          let (mae_parte_1, mae_matching_section, mae_parte_2) = cortarLista pontos mae

          (fazerMatching (pai_parte_1, mae_matching_section, pai_parte_2) pai_matching_section, fazerMatching (mae_parte_1, pai_matching_section, mae_parte_2) mae_matching_section)

          where
            cortarLista :: (Int, Int) -> [a] -> ([a], [a], [a])
            cortarLista (a, b) lista = do
              let (parte_1, resto) = splitAt a lista
                  (parte_2, parte_3) = splitAt (b - a) resto

              (parte_1, parte_2, parte_3)

            fazerMatching :: Eq a => ([a], [a], [a]) -> [a] -> [a]
            fazerMatching (parte_1, match, parte_2) matchSection = fazerMatching' parte_1 matchSection ++ match ++ fazerMatching' parte_2 matchSection
              where
                fazerMatching' :: Eq a => [a] -> [a] -> [a]
                fazerMatching' [] _ = []
                fazerMatching' (a:as) matchSection' 
                  | Just i <- elemIndex a matchSection' = [matchSection' !! i]
                  | otherwise = a : fazerMatching' as matchSection'


    gerarNumeroAleatorio :: Int -> Int -> IO (Int, Int)
    gerarNumeroAleatorio a b 
      | a == b = error "Ta de sacanagem mane, dois numeros iguais?"
      | otherwise = gerarNumeroAleatorio' a b
      where
        gerarNumeroAleatorio' :: Int -> Int -> IO (Int, Int)
        gerarNumeroAleatorio' a' b' = do
          a'' <- randomInt (a', b')
          b'' <- randomInt (a', b')

          if a'' == b'' then
            gerarNumeroAleatorio' a b
          else
            return $ retornarMaior a'' b''

        retornarMaior :: Int -> Int -> (Int, Int)
        retornarMaior a' b' 
          | a' > b' = (b', a')
          | otherwise = (a', b')