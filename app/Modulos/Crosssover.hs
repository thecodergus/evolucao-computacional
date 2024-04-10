module Crosssover where


import Tipos (Individuo(genes, Individuo), Populacao)
import Aleatoriedades (randomInt)


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

crossover :: Ord a => Populacao a -> (Individuo a -> Individuo a -> IO (Individuo a, Individuo a)) -> IO (Populacao a)
crossover [] _ = return []
crossover [a] _ = return [a]
crossover populacao estrategiaCrossover  = do
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
  
  -- Realiza o crossover entre o pai e a mãe
  (maisVelho, maisNovo) <- estrategiaCrossover pai mae

  restante <- crossover populacao3 estrategiaCrossover

  return $ maisVelho : maisNovo : restante


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