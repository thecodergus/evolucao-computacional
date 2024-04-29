module Mutacao where

import Tipos (Individuo(genes, Individuo))
import Utils.Aleatoriedades (randomFloat, randomInt)


-- A função 'mutacao' recebe um indivíduo e uma probabilidade de mutação como entrada.
-- Ela retorna um novo indivíduo que é o resultado da aplicação da mutação ao indivíduo de entrada.
mutacao :: Individuo a -> Float -> IO (Individuo a)
mutacao individuo probabilidade_mutacao = do
  -- Aplica a função 'mutacao'' aos genes do indivíduo.
  novo_gene <- mutacao' (genes individuo)
  -- Retorna um novo indivíduo com os genes mutados e fitness 0.
  return (Individuo novo_gene 0)
  where
    -- A função 'mutacao'' recebe uma lista de genes e retorna uma nova lista de genes
    -- onde cada gene foi possivelmente mutado.
    mutacao' = mapM mutar

    -- A função 'mutar' recebe um gene e retorna um novo gene.
    -- Com probabilidade 'probabilidade_mutacao', o gene é substituído por um gene aleatório.
    -- Caso contrário, o gene original é retornado.
    mutar item = do
      -- Gera um número aleatório entre 0 e 1.
      prob <- randomFloat (0, 1)
      if probabilidade_mutacao >= prob
        then do
          -- Se a probabilidade de mutação for maior ou igual ao número aleatório,
          -- substitui o gene por um gene aleatório da lista de genes do indivíduo.
          indice <- randomInt (0, length (genes individuo) - 1)
          return (genes individuo !! indice)
        else -- Caso contrário, retorna o gene original.
          return item

-- A função bitflip realiza a mutação bit flip em um cromossomo binário de um indivíduo
-- Ela recebe um indivíduo do tipo Individuo Bool e uma probabilidade de mutação
-- e retorna um novo indivíduo com o cromossomo mutado
bitflip :: Individuo Bool -> Float -> IO (Individuo Bool)
bitflip (Individuo gene _) probabilidade = do
  -- Realiza a mutação do gene usando a função mutar
  genes' <- mutar gene
  -- Retorna um novo indivíduo com o cromossomo mutado e aptidão definida como 0.0
  return $ Individuo genes' 0.0
  where
    -- A função mutar percorre o cromossomo do indivíduo e decide se cada bit deve ser mutado ou não
    mutar :: [Bool] -> IO [Bool]
    -- Caso base: se o cromossomo estiver vazio, retorna uma lista vazia
    mutar [] = return []
    -- Caso recursivo: processa o primeiro bit e depois chama mutar nos bits restantes
    mutar (x : xs) = do
      -- Gera um número aleatório entre 0 e 1
      prob <- randomFloat (0, 1)
      -- Chama mutar nos bits restantes
      resto <- mutar xs
      -- Se a probabilidade gerada for menor ou igual à probabilidade de mutação,
      -- inverte o bit atual e adiciona aos bits restantes
      if prob <= probabilidade
        then return $ not x : resto
        else -- Senão, mantém o bit atual e adiciona aos bits restantes
          return $ x : resto