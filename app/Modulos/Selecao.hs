module Selecao where

import Utils.Aleatoriedades (randomInt, randomFloat)
import Control.Monad (replicateM)
import Utils.Avaliacoes (vencedorDoTorneio)
import Tipos (Populacao, Individuo(fitness, Individuo))


roletaComReposicao :: Populacao a -> IO (Populacao a)
roletaComReposicao pop = girarRoleta (fitnessRelativo pop (fitnessTotal pop)) (length pop)
  where
    fitnessTotal :: Populacao a -> Float
    fitnessTotal pop' = sum $ map fitness pop'

    fitnessRelativo :: Populacao a -> Float -> Populacao a
    fitnessRelativo pop' total = map (\(Individuo g fit) -> Individuo g (fit / total)) pop'

    selecionarIndividuo :: Populacao a -> Float -> Maybe (Individuo a)
    selecionarIndividuo [] _ = Nothing
    selecionarIndividuo (individuo@(Individuo _ fit) : pop') valorSorteado
      | fit <= valorSorteado = Just individuo
      | otherwise = selecionarIndividuo pop' valorSorteado

    girarRoleta :: Populacao a -> Int -> IO (Populacao a)
    girarRoleta [] _ = return []
    girarRoleta pop' 0 = return pop'
    girarRoleta pop' numVezes = do
      jogarBolinha <- randomFloat (0, 1)

      case selecionarIndividuo pop' jogarBolinha of
        Nothing -> girarRoleta pop' numVezes
        Just individuo -> do
          restante <- girarRoleta pop' (numVezes - 1)

          return $ individuo : restante

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
