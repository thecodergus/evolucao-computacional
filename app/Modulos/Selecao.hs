module Selecao where

import Utils.Aleatoriedades (randomInt, randomFloat)
import Utils.Outros(shuffle)
import Control.Monad (replicateM)
import Utils.Avaliacoes (vencedorDoTorneio)
import Tipos (Populacao, Individuo(fitness, Individuo))


-- roletaComReposicao :: Populacao a -> IO (Populacao a)
-- roletaComReposicao pop = girarRoleta (fitnessRelativo pop (fitnessTotal pop)) (length pop)
--   where
--     fitnessTotal :: Populacao a -> Float
--     fitnessTotal pop' = sum $ map fitness pop'

--     fitnessRelativo :: Populacao a -> Float -> Populacao a
--     fitnessRelativo pop' total = map (\(Individuo g fit) -> Individuo g (fit / total)) pop'


--     girarRoleta :: Populacao a -> Int -> IO (Populacao a)
--     girarRoleta [] _ = return []
--     girarRoleta pop' numVezes = do
--       jogarBolinha <- randomFloat (0, 1)

--       case selecionarIndividuo pop' jogarBolinha of
--         Nothing -> girarRoleta pop' numVezes
--         Just individuo -> do
--           restante <- girarRoleta pop' (numVezes - 1)

--           return $ individuo : restante
--       where
--         selecionarIndshuffleividuo todos@(individuo@(Individuo _ fit) : pop') valorSorteado
--           | fit <= valorSorteado = Just individuo
--           | otherwise = selecionarIndividuo todos valorSorteado



-- roletaSemReposicao :: Eq a => Populacao a -> IO (Populacao a)
-- roletaSemReposicao populacao = do 
--   poulacaoIntermediaria <- girarRoleta (fitnessRelativo populacao (fitnessTotal populacao)) (length  populacao)

--   girarRoleta (fitnessRelativo poulacaoIntermediaria (fitnessTotal poulacaoIntermediaria)) (length poulacaoIntermediaria)
--     where
--       fitnessTotal :: Populacao a -> Float
--       fitnessTotal populacao' = sum $ map fitness populacao'

--       fitnessRelativo :: Populacao a -> Float -> Populacao a
--       fitnessRelativo populacao' totalFitness = map (`calcularFitnessRelativo` totalFitness) populacao'
--         where
--           calcularFitnessRelativo (Individuo gene fit) totalFitness' = Individuo gene (fit / totalFitness')

--       girarRoleta :: Eq a => Populacao a -> Int -> IO (Populacao a)
--       girarRoleta [] _ = return []
--       girarRoleta pop contador = do
--         jogarBolinha <- randomFloat (0, 1)
--         case selecionarIndividuo pop jogarBolinha of
--           Nothing -> girarRoleta pop contador
--           Just individuo -> do
--             if contador > 0 then do
--               restante <- girarRoleta (filter (/= individuo) pop) (contador - 1)
--               return $ individuo : restante
--             else
--               return [individuo]

--           where
--             selecionarIndividuo :: Populacao a -> Float -> Maybe (Individuo a)
--             selecionarIndividuo [] _ = Nothing
--             selecionarIndividuo (individuo@(Individuo _ fit) : pop') valorSorteado
--               | fit <= valorSorteado = Just individuo
--               | otherwise = selecionarIndividuo pop' valorSorteado


-- Algoritmo:
-- 1. Calcular o fitness total
-- 2. Calcular o fitness relativo sobre cada Individuo da população
-- 3. Sortear um valor aleatorio entre 0 e fitness total
-- 4. Embaralhar a lista de Individuos (População)
-- 5. Interar sobre a lista indo somando os fitness relativos a partir de zero até que o acumulador seja igual ou superior ao valor aleatorio
-- 6. Sobre a nova população aplicar os passos 1 ao 5 novamente. Pois essa primeira população é a população intermediaria
-- 7. Retornar a população final
roletaSemReposicao :: Populacao a -> IO (Populacao a)
roletaSemReposicao populacao = do
  -- Gerar valor aleatorio da roleta para a população intermediaria
  valorAleatorio' <- randomFloat (0, fitnessTotal populacao)

  -- Calcular fitness Relativo
  let populacao' = calcularFitnessRelativo populacao

  -- Procurar pela população intermediaria
  populacaoIntermediaria <- girarRoleta populacao' valorAleatorio'

  let populacao'' = calcularFitnessRelativo populacaoIntermediaria

  -- Gerar o valor aleatorio da role para a população final
  valorAleatorio'' <- randomFloat (0, fitnessTotal populacao'')

  -- Procurando pela populacao final
  girarRoleta populacao'' valorAleatorio''

    where
      -- Função auxiliar para calcular o fitness total da população
      fitnessTotal :: Populacao a -> Float
      fitnessTotal pop = sum $ map fitness pop

      -- Função auxiliar para calcular o fitness relativo de cada individuo
      calcularFitnessRelativo :: Populacao a -> Populacao a
      calcularFitnessRelativo pop = map (\(Individuo gene fit) -> Individuo gene (fit / fitnessTotal pop)) pop

      -- Função auxiliar que gira roleta da sorte
      girarRoleta :: Populacao a -> Float -> IO (Populacao a)
      girarRoleta pop valor = do
        pop' <- shuffle pop
        return $ encontrarIndividuos pop' valor
          where
            -- Função auxiliar para encontra os individuos que serão selecionados na roleta
            encontrarIndividuos :: Populacao a -> Float -> Populacao a
            encontrarIndividuos [] _ = []
            encontrarIndividuos (p : ps) valor'
              | fitness p <= valor' = p : encontrarIndividuos ps (valor' - fitness p)
              | fitness p > valor' && valor' > 0 = [p]
              | otherwise = []






-- A função torneioEstocastico recebe o tamanho do torneio (k), a taxa de seleção (kp) e uma população como parâmetros
torneioEstocastico :: (Eq a, Ord a) => Int -> Float -> Populacao a -> IO (Populacao a)
torneioEstocastico k kp populacao
  -- Se k for menor que 2, retorna um erro informando que o valor mínimo é 2
  | k < 2 = error "O valor minimo de k eh igual a 2"
  | otherwise = do
    -- Calcula o tamanho da população e a quantidade de indivíduos a serem selecionados com base na taxa de seleção
    let tamanhoPopulacao = length populacao
        qtdSelecionados = ceiling (fromIntegral tamanhoPopulacao * kp)

    -- Para cada indivíduo a ser selecionado, seleciona k índices aleatoriamente para formar um torneio
    torneios <- replicateM qtdSelecionados (replicateM k (randomInt (0, tamanhoPopulacao - 1)))

    -- Realiza o torneio para cada grupo de índices selecionados e retorna a lista de vencedores
    mapM (vencedorDoTorneio populacao) torneios

torneio :: (Ord a) => Int -> Populacao a -> IO (Populacao a)
torneio k populacao
  | k < 2 = error "O valor minimo de k eh igual a 2"
  | otherwise = do
    -- Calcula o tamanho da população
    let tamanhoPopulacao = length populacao

    -- Para cada indivíduo da população, seleciona k índices aleatoriamente para formar um torneio
    torneios <- replicateM tamanhoPopulacao (replicateM k (randomInt (0, tamanhoPopulacao - 1)))

    -- Realiza o torneio para cada grupo de índices selecionados e retorna a lista de vencedores
    mapM (vencedorDoTorneio populacao) torneios
