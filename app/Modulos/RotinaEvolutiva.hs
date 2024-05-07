module RotinaEvolutiva where


import Mutacao(mutacao)
import Crosssover(umPontoAleatorio)
import Tipos (GeracaoInfo (GeracaoInfo, elitistas, mediaFitness), Individuo (Individuo), Populacao)
import Control.Parallel.Strategies (parMap, rpar)
import Data.Maybe (maybeToList)
import Utils.Avaliacoes (melhorIndividuo)
import Utils.Outros (shuffle)
import Utils.Aleatoriedades (selecionarRemoverRandom)

-- Retorna a ultima População e o historico de melhores individuos
loopEvolutivoEnumerado :: (Ord a) => Populacao a -> (Individuo a -> Individuo a) -> (Populacao a -> IO (Populacao a)) -> (Individuo a -> IO (Individuo a)) -> ((Individuo a, Individuo a) -> IO (Individuo a, Individuo a)) -> Float -> Int -> IO (GeracaoInfo a)
loopEvolutivoEnumerado _ _ _ _ _ _ 0 = return (GeracaoInfo [] [])
loopEvolutivoEnumerado populacao funcaoAvaliacao funcaoSelecao funcaMutacao funcaoCrossover generatioGap contador = do
    print $ "----------- Loop Evolutivo Enumerado numero " ++ show contador ++ "-----------"

    -- Avaliacao
    let populacaoAvaliada = avaliarPopoulacao populacao

    -- Encontrar o melhor individuo
    let individuoEletista = maybeToList $ melhorIndividuo populacaoAvaliada

    -- Ativando a questão do generatioGap
    (veios, novinhos) <- selecionarQuemFica generatioGap populacaoAvaliada
    
    -- Selecao
    individuosSelecionados <- funcaoSelecao novinhos

    -- Crossover
    novaPopulacao <- crossover individuosSelecionados funcaoCrossover

    -- Mutacao
    novaPopulacao' <- mutarPopulacao novaPopulacao

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
        crossover :: Ord a => Populacao a -> ((Individuo a, Individuo a) -> IO (Individuo a, Individuo a)) -> IO (Populacao a)
        crossover [] _ = return []
        crossover [a] _ = return [a]
        crossover pop estrategiaCrossover = do
            resultPai <- selecionarRemoverRandom pop
            case resultPai of
                Nothing -> error "Erro ao selecionar pai durante o crossover"
                Just (pai, pop') -> do
                    resultMae <- selecionarRemoverRandom pop'
                    case resultMae of
                        Nothing -> error "Erro ao selecionar mae durante o crossover"
                        Just (mae, pop'') -> do
                            restante <- crossover pop'' estrategiaCrossover
                            (maisVelho, maisNovo) <- estrategiaCrossover (pai, mae)
                            return $ maisVelho : maisNovo : restante
