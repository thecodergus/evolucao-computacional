module RotinaEvolutiva where


import Mutacao(mutacao)
import Crosssover(umPontoAleatorio)
import Tipos (GeracaoInfo (GeracaoInfo, mediaPopulacao, melhorIndividuo, piorIndividuo), Individuo (Individuo), Populacao)
import Control.Parallel.Strategies (parMap, rpar)
import Data.Maybe (maybeToList)
import qualified Utils.Avaliacoes as Utils
import Utils.Outros (shuffle)
import Utils.Aleatoriedades (selecionarRemoverRandom)
import Debug.Trace(trace)

-- Retorna a ultima População e o historico de melhores individuos
loopEvolutivoEnumerado :: (Ord a, Show a) =>
    Populacao a -> -- População inicial
    (Individuo a -> Individuo a) -> -- Função de avaliação
    (Populacao a -> IO (Populacao a)) -> -- Função de seleção
    (Individuo a -> IO (Individuo a)) ->  -- Função de mutação
    ((Individuo a, Individuo a) -> IO (Individuo a, Individuo a)) -> -- Função de crossover
    Float -> -- Generatio Gap
    Int -> -- Numero de interações
    IO (GeracaoInfo a)
loopEvolutivoEnumerado _ _ _ _ _ _ 0 = return (GeracaoInfo [] [] [])
loopEvolutivoEnumerado populacao funcaoAvaliacao funcaoSelecao funcaMutacao funcaoCrossover generatioGap contador =
    let populacaoAvaliada = avaliarPopoulacao populacao funcaoAvaliacao
        individuoEletista = maybeToList $ Utils.melhorIndividuo populacaoAvaliada
    in  trace ("-------- Loop Evolutivo Geracao " ++ show contador ++ "-------") $ selecionarQuemFica generatioGap populacaoAvaliada
        -- Selecionando os individuos que ficam e os que morrem
        >>= \(veios, novinhos) ->
            trace ("Velhos | Novos => " ++ show (length veios) ++ " | " ++ show (length novinhos)) $
            -- Selecionando os individuos que ficam
            funcaoSelecao novinhos
                >>= \individuosSelecionados ->
                trace ("Individuos Selecionados => " ++ show (length individuosSelecionados)) $
                -- Realizando o crossover
                    crossover individuosSelecionados funcaoCrossover (length individuosSelecionados)
                    >>= \novaPopulacao ->
                        trace ("Nova Populacao => " ++ show (length novaPopulacao)) $
                        -- Realizando a mutação
                        mutarPopulacao novaPopulacao funcaMutacao
                            >>= \novaPopulacao' ->
                            trace ("Populacao Mutada => " ++ show (length novaPopulacao')) $
                            -- Chamando recursivamente a proxima interação
                                loopEvolutivoEnumerado (individuoEletista ++ novaPopulacao' ++ veios) funcaoAvaliacao funcaoSelecao funcaMutacao funcaoCrossover generatioGap (contador - 1)
                                >>= \proximaGeracao ->
                                    return $ GeracaoInfo (individuoEletista ++ melhorIndividuo proximaGeracao) (calcularMediaFitness populacaoAvaliada : mediaPopulacao proximaGeracao) (maybeToList (Utils.piorIndividuo populacaoAvaliada) ++ piorIndividuo proximaGeracao)

    where
        -- Função auxiliar para calcular a media de fitness de cada geração
        calcularMediaFitness :: Populacao a -> Float
        calcularMediaFitness pop = foldl (\acc (Individuo _ fitness') -> acc + fitness' ) 0.0 pop / fromIntegral (length pop)

        -- Função auxiliar para calcular o fitness de uma população de forma paralela
        avaliarPopoulacao :: Populacao a -> (Individuo a -> Individuo a) -> Populacao a
        avaliarPopoulacao pop funcaoAvaliacao' = parMap rpar funcaoAvaliacao' pop

        -- Função auxliar para realizar a mutação em uma determinada população
        mutarPopulacao :: Populacao a -> (Individuo a -> IO (Individuo a)) -> IO (Populacao a)
        mutarPopulacao pop funcaMutacao' = sequence $ parMap rpar funcaMutacao' pop


        -- Função auxiliar para selecionar os ficaram e os que morreram de uma determinada população na virada geracional
        selecionarQuemFica :: Float -> Populacao a -> IO (Populacao a, Populacao a)
        selecionarQuemFica 1 pop = return ([], pop)
        selecionarQuemFica gap pop = shuffle pop >>= \pop' -> return $ splitAt (round $ gap * fromIntegral (length pop)) pop'

        -- Função auxiliar para selecionar os pais e realizar o crossover entre eles
        crossover :: Populacao a -> ((Individuo a, Individuo a) -> IO (Individuo a, Individuo a)) -> Int -> IO (Populacao a)
        crossover [] _  _ = return []
        crossover _ _  0 = return []
        crossover _ _  1 = return []
        crossover [x] _  _ = return [x]
        crossover (pai : mae : pop) funcaoCrossover' cont =
            -- Realizando o crossover
            funcaoCrossover' (pai, mae) >>= \(filho1, filho2) ->

            -- Chamando recursivamente
            crossover pop funcaoCrossover' (cont - 2) >>= \proximosFilhos ->

            -- Retornando valores
            return $ filho1 : filho2 : proximosFilhos

