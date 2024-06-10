module RotinaEvolutiva where

import Control.Monad ((>=>))
import Control.Parallel.Strategies (parMap, rpar)
import Crosssover (umPontoAleatorio)
import Data.Maybe (maybeToList)
import Debug.Trace (trace)
import Mutacao (mutacao)
import Tipos (GeracaoInfo (GeracaoInfo, mediaPopulacao, melhorIndividuo, piorIndividuo), Individuo (Individuo), Populacao)
import Utils.Aleatoriedades (selecionarRemoverRandom)
import qualified Utils.Avaliacoes as Utils
import Utils.Outros (pairToList, removeElements, shuffle, toPairs)

-- Retorna a ultima População e o historico de melhores individuos
loopEvolutivoEnumerado ::
  (Ord a, Show a) =>
  Populacao a -> -- População inicial
  (Individuo a -> Individuo a) -> -- Função de avaliação
  (Populacao a -> IO (Individuo a)) -> -- Função de seleção
  (Individuo a -> IO (Individuo a)) -> -- Função de mutação
  ((Individuo a, Individuo a) -> IO (Individuo a, Individuo a)) -> -- Função de crossover
  Float -> -- Generatio Gap
  Int -> -- Numero de interações
  IO (GeracaoInfo a)
loopEvolutivoEnumerado _ _ _ _ _ _ 0 = return (GeracaoInfo [] [] [])
loopEvolutivoEnumerado populacao funcaoAvaliacao funcaoSelecao funcaMutacao funcaoCrossover generatioGap contador =
  let populacaoAvaliada = avaliarPopoulacao populacao funcaoAvaliacao
      individuoEletista = maybeToList $ Utils.melhorIndividuo populacaoAvaliada
   in -- trace ("Geracao (" ++ show contador ++ ")")
      selecionarQuemFica generatioGap populacaoAvaliada
        >>=
        -- Selecionando os individuos que ficam e os que morrem
        \(veios, novinhos) ->
          -- Selecionando os individuos que ficam
          selecionarPais novinhos [] funcaoSelecao (length novinhos)
            >>= \paiSelecionados ->
              -- Realizando o crossover
              crossover paiSelecionados funcaoCrossover
                >>= \novaPopulacao ->
                  -- Realizando a mutação
                  mutarPopulacao novaPopulacao funcaMutacao
                    >>= \novaPopulacao' ->
                      -- Chamando recursivamente a proxima interação
                      loopEvolutivoEnumerado (individuoEletista ++ novaPopulacao' ++ veios) funcaoAvaliacao funcaoSelecao funcaMutacao funcaoCrossover generatioGap (contador - 1)
                        >>= \proximaGeracao ->
                          return $ GeracaoInfo (individuoEletista ++ melhorIndividuo proximaGeracao) (calcularMediaFitness populacaoAvaliada : mediaPopulacao proximaGeracao) (maybeToList (Utils.piorIndividuo populacaoAvaliada) ++ piorIndividuo proximaGeracao)
  where
    -- Função auxiliar para calcular a media de fitness de cada geração
    calcularMediaFitness :: Populacao a -> Float
    calcularMediaFitness pop = foldl (\acc (Individuo _ fitness') -> acc + fitness') 0.0 pop / fromIntegral (length pop)

    -- Função auxiliar para calcular o fitness de uma população de forma paralela
    avaliarPopoulacao :: Populacao a -> (Individuo a -> Individuo a) -> Populacao a
    avaliarPopoulacao pop funcaoAvaliacao' = parMap rpar funcaoAvaliacao' pop

    -- Função auxliar para realizar a mutação em uma determinada população
    mutarPopulacao :: Populacao a -> (Individuo a -> IO (Individuo a)) -> IO (Populacao a)
    mutarPopulacao pop funcaMutacao' = sequence $ parMap rpar funcaMutacao' pop

    -- Função auxiliar para selecionar os ficaram e os que morreram de uma determinada população na virada geracional
    selecionarQuemFica :: Float -> Populacao a -> IO (Populacao a, Populacao a)
    selecionarQuemFica 1 pop = return ([], pop)
    selecionarQuemFica gap pop =
      shuffle pop
        >>= \pop' -> return $ splitAt (round $ gap * fromIntegral (length pop)) pop'

    -- Função auxiliar para selecionar os pais e realizar o crossover entre eles de forma paralela
    -- Estranhamente esta função é mais lerda do que a primeira
    crossover :: (Show a) => [(Individuo a, Individuo a)] -> ((Individuo a, Individuo a) -> IO (Individuo a, Individuo a)) -> IO (Populacao a)
    crossover pop f = pairToList . concat <$> sequence (parMap rpar (f >=> return . (: [])) pop)

    selecionarPais :: Eq a => Populacao a -> Populacao a -> (Populacao a -> IO (Individuo a)) -> Int -> IO [(Individuo a, Individuo a)]
    selecionarPais pop ultimosIndividuos f contador'
      | contador' > 0 && even contador' =
        f (removeElements ultimosIndividuos pop)
          >>= \a ->
            f (removeElements (ultimosIndividuos ++ [a]) pop)
              >>= \b ->
                selecionarPais pop [a, b] f (contador' - 2)
                  >>= \result -> return $ (a, b) : result
      | otherwise = return []
