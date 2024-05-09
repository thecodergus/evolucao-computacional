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
    let populacaoAvaliada = avaliarPopoulacao populacao

    -- Encontrar o melhor individuo
    let individuoEletista = maybeToList $ melhorIndividuo populacaoAvaliada

    -- Ativando a questão do generatioGap
    (veios, novinhos) <- selecionarQuemFica generatioGap populacaoAvaliada

    -- Selecao
    individuosSelecionados <- trace ("Velhos | Novos => " ++ show (length veios) ++ " | " ++ show (length novinhos)) $ funcaoSelecao novinhos

    -- Crossover
    novaPopulacao <- trace ("Individuos Selecionados => " ++ show (length individuosSelecionados)) $ crossover individuosSelecionados funcaoCrossover [] contador

    -- Mutacao
    novaPopulacao' <- trace ("Nova Populacao => " ++ show (length novaPopulacao)) $ mutarPopulacao novaPopulacao

    -- Ordernar nova interação no Loop evolutivo
    proximaGeracao <- loopEvolutivoEnumerado (individuoEletista ++ novaPopulacao') funcaoAvaliacao funcaoSelecao funcaMutacao funcaoCrossover generatioGap (contador - 1)

    -- Retornando valores
    return $ GeracaoInfo (individuoEletista ++ veios ++ elitistas proximaGeracao) (calcularMediaFitness populacaoAvaliada : mediaFitness proximaGeracao)


    where
        -- Função auxiliar para calcular a media de fitness de cada geração
        calcularMediaFitness :: Populacao a -> Float
        calcularMediaFitness pop = foldl (\acc (Individuo _ fitness') -> acc + fitness' ) 0.0 pop / fromIntegral (length pop)

        -- Função auxiliar para calcular o fitness de uma população de forma paralela
        avaliarPopoulacao = parMap rpar funcaoAvaliacao

        -- Função auxliar para realizar a mutação em uma determinada população
        -- mutarPopulacao :: Populacao a -> IO (Populacao a)
        mutarPopulacao pop = sequence $ parMap rpar funcaMutacao pop


        -- Função auxiliar para selecionar os ficaram e os que morreram de uma determinada população na virada geracional
        selecionarQuemFica :: Float -> Populacao a -> IO (Populacao a, Populacao a)
        selecionarQuemFica 1 pop = return ([], pop)
        selecionarQuemFica gap pop = do
            pop' <- shuffle pop

            return $ splitAt (round $ gap * fromIntegral (length pop)) pop'

        -- Função auxiliar para selecionar os pais e realizar o crossover entre eles
        crossover :: (Ord a, Eq a) => Populacao a -> ((Individuo a, Individuo a) -> IO (Individuo a, Individuo a)) -> [(Individuo a, Individuo a)] -> Int -> IO (Populacao a)
        crossover [] _ _ _ = return []
        crossover [a] _ _ _ = return [a]
        crossover _ _ _ 0 = return []
        crossover pop funcaoCrossover' listaDePais contador' = do
            resultadoPai <- selecionarRemoverRandom pop
            case resultadoPai of
                Nothing -> return []
                Just (pai, pop') -> do
                    resultadoMae <- selecionarRemoverRandom pop'
                    case resultadoMae of
                        Nothing -> return []
                        Just (mae, _) -> do
                            if any (\a -> a == (pai, mae) || a == (mae, pai)) listaDePais then do
                                -- Vou matar o pai para evitar que ele pegue a mãe de novo
                                crossover (filter (/=pai) pop) funcaoCrossover' listaDePais contador'
                            else do
                                (maisVelho, maisNovo) <- funcaoCrossover' (pai, mae)
                                restante <- crossover pop funcaoCrossover' ((pai, mae) : listaDePais) (contador' - 1)

                                return $ maisVelho : maisNovo : restante
