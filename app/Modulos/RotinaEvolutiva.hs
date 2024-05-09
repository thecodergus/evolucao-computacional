module RotinaEvolutiva where


import Mutacao(mutacao)
import Crosssover(umPontoAleatorio)
import Tipos (GeracaoInfo (GeracaoInfo, elitistas, mediaFitness), Individuo (Individuo), Populacao)
import Control.Parallel.Strategies (parMap, rpar)
import Data.Maybe (maybeToList)
import Utils.Avaliacoes (melhorIndividuo)
import Utils.Outros (shuffle)
import Utils.Aleatoriedades (selecionarRemoverRandom)
import Debug.Trace(trace)

-- Retorna a ultima População e o historico de melhores individuos
loopEvolutivoEnumerado :: (Ord a) => Populacao a -> (Individuo a -> Individuo a) -> (Populacao a -> IO (Populacao a)) -> (Individuo a -> IO (Individuo a)) -> ((Individuo a, Individuo a) -> IO (Individuo a, Individuo a)) -> Float -> Int -> IO (GeracaoInfo a)
loopEvolutivoEnumerado _ _ _ _ _ _ 0 = return (GeracaoInfo [] [])
loopEvolutivoEnumerado populacao funcaoAvaliacao funcaoSelecao funcaMutacao funcaoCrossover generatioGap contador = do
    print $ "----------- Loop Evolutivo Enumerado numero " ++ show contador ++ "-----------"
    print $ "Tamanho da populacao => " ++ show (length populacao)

    -- Avaliacao
    let populacaoAvaliada = avaliarPopoulacao populacao funcaoAvaliacao

    -- Encontrar o melhor individuo
    let individuoEletista = maybeToList $ melhorIndividuo populacaoAvaliada 

    -- Ativando a questão do generatioGap
    (veios, novinhos) <- selecionarQuemFica generatioGap populacaoAvaliada

    -- Selecao
    individuosSelecionados <- trace ("Velhos | Novos => " ++ show (length veios) ++ " | " ++ show (length novinhos)) $ funcaoSelecao novinhos

    -- Crossover
    novaPopulacao <- trace ("Individuos Selecionados => " ++ show (length individuosSelecionados)) $ crossover individuosSelecionados funcaoCrossover (length individuosSelecionados)

    -- Mutacao
    novaPopulacao' <- trace ("Nova Populacao => " ++ show (length novaPopulacao)) $ mutarPopulacao novaPopulacao funcaMutacao

    -- Ordernar nova interação no Loop evolutivo
    proximaGeracao <- loopEvolutivoEnumerado (individuoEletista ++ novaPopulacao') funcaoAvaliacao funcaoSelecao funcaMutacao funcaoCrossover generatioGap (contador - 1)

    -- Retornando valores
    return $ GeracaoInfo (individuoEletista ++ elitistas proximaGeracao) (calcularMediaFitness populacaoAvaliada : mediaFitness proximaGeracao)


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
        selecionarQuemFica gap pop = do
            pop' <- shuffle pop

            return $ splitAt (round $ gap * fromIntegral (length pop)) pop'

        -- Função auxiliar para selecionar os pais e realizar o crossover entre eles
        crossover :: Populacao a -> ((Individuo a, Individuo a) -> IO (Individuo a, Individuo a)) -> Int -> IO (Populacao a)
        crossover [] _  _ = return []
        crossover _ _  0 = return []
        crossover _ _  1 = return []
        crossover [x] _  _ = return [x]
        crossover pop funcaoCrossover' cont = do
            -- Selecionando pai
            Just (pai, pop') <- selecionarRemoverRandom pop

            -- Selecionando mae
            Just (mae, _) <- selecionarRemoverRandom pop'

            -- Realizando o crossover
            (filho1, filho2) <- funcaoCrossover' (pai, mae)

            -- Chamando recursivamente
            proximosFilhos <- crossover pop funcaoCrossover' (cont - 2)

            -- Retornando valores
            return $ filho1 : filho2 : proximosFilhos
            
