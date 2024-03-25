module Mutacao where

import Tipos (Individuo(genes, Individuo))
import GerarAleatoriedades (randomFloat, randomInt)


-- A função 'mutacao' recebe um indivíduo e uma probabilidade de mutação como entrada.
-- Ela retorna um novo indivíduo que é o resultado da aplicação da mutação ao indivíduo de entrada.
mutacao :: Individuo (*) -> Float -> IO (Individuo (*))
mutacao individuo probabilidade_mutacao = do
  -- Aplica a função 'mutacao'' aos genes do indivíduo.
  novo_individuo <- mutacao' (genes individuo)
  -- Retorna um novo indivíduo com os genes mutados e fitness 0.
  return (Individuo novo_individuo 0)
  where
    -- A função 'mutacao'' recebe uma lista de genes e retorna uma nova lista de genes
    -- onde cada gene foi possivelmente mutado.
    mutacao' :: [*] -> IO [*]
    mutacao' xs = mapM mutar xs

    -- A função 'mutar' recebe um gene e retorna um novo gene.
    -- Com probabilidade 'probabilidade_mutacao', o gene é substituído por um gene aleatório.
    -- Caso contrário, o gene original é retornado.
    mutar :: * -> IO *
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
