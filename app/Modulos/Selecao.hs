module Selecao where

import Control.Monad
import Control.Monad (replicateM)
import Control.Parallel.Strategies (parMap, rpar)
import Data.Foldable (maximumBy)
import Data.List (minimumBy, sort)
import Data.Maybe (fromMaybe)
import Data.Ord (comparing)
import Debug.Trace (trace)
import Tipos (Individuo (Individuo, fitness), Populacao)
import Utils.Aleatoriedades (escolherRandoms, randomFloat, randomInt, selecionarRemoverRandom)
import Utils.Avaliacoes (vencedorDoTorneio)
import Utils.Outros (shuffle)

roletaComReposicao :: (Ord a, Show a) => Populacao a -> IO (Populacao a)
roletaComReposicao populacao = girarRoleta (fitnessRelativo populacao) (length populacao)
  where
    fitnessTotal :: Populacao a -> Float
    fitnessTotal pop = sum $ map fitness pop

    fitnessRelativo pop = sort $ parMap rpar calcularFitnessRelativo pop
      where
        calcularFitnessRelativo :: Individuo a -> Individuo a
        calcularFitnessRelativo individuo = individuo {fitness = 360 * (fitness individuo / (if fitnessTotal pop /= 0 then fitnessTotal pop else 1))}

    girarRoleta :: (Ord a, Show a) => Populacao a -> Int -> IO (Populacao a)
    girarRoleta [] _ = return []
    girarRoleta _ 0 = return []
    girarRoleta pop n =
      randomFloat (0, 360) >>= \sorteio -> (encontrarIndividuo pop sorteio 0 :) <$> girarRoleta pop (n - 1)
      where
        encontrarIndividuo :: Populacao a -> Float -> Float -> Individuo a
        encontrarIndividuo [] _ _ = error "Individuo nao encontrado"
        encontrarIndividuo [p] _ _ = p
        encontrarIndividuo (p : ps) sorteio acc
          | sorteio <= acc + fitness p = p
          | otherwise = encontrarIndividuo ps sorteio (acc + fitness p)

roletaSemReposicao :: (Ord a, Show a, Eq a) => Populacao a -> IO (Individuo a)
roletaSemReposicao populacao = girarRoleta populacao Nothing
  where
    fitnessTotal :: Populacao a -> Float
    fitnessTotal pop = sum $ map fitness pop

    fitnessRelativo pop = sort $ parMap rpar calcularFitnessRelativo pop
      where
        calcularFitnessRelativo :: Individuo a -> Individuo a
        calcularFitnessRelativo individuo = individuo {fitness = 360 * (fitness individuo / (if fitnessTotal pop /= 0 then fitnessTotal pop else 1))}

    girarRoleta :: (Ord a, Show a, Eq a) => Populacao a -> Maybe (Individuo a) -> IO (Individuo a)
    girarRoleta [] _ = error "Lista vazia"
    girarRoleta pop indi =
      randomFloat (0, 360)
        >>= \sorteio -> return $ encontrarIndividuo (fitnessRelativo (removerAntigoIndividuoSelecionado pop indi)) sorteio 0
      where
        encontrarIndividuo :: Populacao a -> Float -> Float -> Individuo a
        encontrarIndividuo [] _ _ = error "Individuo nao encontrado"
        encontrarIndividuo [p] _ _ = p
        encontrarIndividuo (p : ps) sorteio acc
          | sorteio <= acc + fitness p = p
          | otherwise = encontrarIndividuo ps sorteio (acc + fitness p)

        removerAntigoIndividuoSelecionado :: Eq a => Populacao a -> Maybe (Individuo a) -> Populacao a
        removerAntigoIndividuoSelecionado pop' Nothing = pop'
        removerAntigoIndividuoSelecionado pop' (Just individuo) = filter (/= individuo) pop'

-- A função torneioEstocastico recebe o tamanho do torneio (k), a taxa de seleção (kp) e uma população como parâmetros
torneioEstocastico :: (Eq a, Ord a) => Int -> Float -> Populacao a -> IO (Populacao a)
torneioEstocastico k kp populacao
  -- Se k for menor que 2, retorna um erro informando que o valor mínimo é 2
  | k < 2 = error "O valor minimo de k eh igual a 2"
  | kp < 0 || kp > 1 = error "O valor de kp deve estar entre 0 e 1"
  | otherwise = campeonato (length populacao) k kp populacao
  where
    campeonato :: Int -> Int -> Float -> Populacao a -> IO (Populacao a)
    campeonato cont k' kp' pop
      | cont <= 0 = return []
      | otherwise =
        escolherRandoms k' pop
          >>= \escolhidos ->
            randomFloat (0, 1)
              >>= \chance ->
                case if chance <= kp' then melhorCampeao escolhidos else piorCampeao escolhidos of
                  Nothing -> return []
                  Just campeao -> campeonato (cont - 1) k' kp' pop >>= \restante -> return $ campeao : restante
      where
        melhorCampeao :: Populacao a -> Maybe (Individuo a)
        melhorCampeao [] = Nothing
        melhorCampeao pop' = Just $ maximumBy (comparing fitness) pop'

        piorCampeao :: Populacao a -> Maybe (Individuo a)
        piorCampeao [] = Nothing
        piorCampeao pop' = Just $ minimumBy (comparing fitness) pop'

torneio :: (Ord a) => Int -> Populacao a -> IO (Populacao a)
torneio k populacao
  | k < 2 = error "O valor minimo de k eh igual a 2"
  | otherwise = campeonato (length populacao) k populacao
  where
    campeonato :: Int -> Int -> Populacao a -> IO (Populacao a)
    campeonato cont k' pop
      | cont <= 0 = return []
      | otherwise =
        escolherRandoms k pop
          >>= \escolhidos ->
            case lutar escolhidos of
              Nothing -> return []
              Just campeao -> campeonato (cont - 1) k' pop >>= \restante -> return $ campeao : restante

    lutar :: Populacao a -> Maybe (Individuo a)
    lutar [] = Nothing
    lutar pop = Just $ maximumBy (comparing fitness) pop