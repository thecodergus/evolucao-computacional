module RotinaEvolutiva where


import Crosssover(selecionarPais)
import Tipos (GeracaoInfo (GeracaoInfo, elitistas, mediaFitness), Individuo (Individuo), Populacao)
import Control.Parallel.Strategies (parMap, rpar)
import Data.Maybe (maybeToList)
import Utils.Avaliacoes (melhorIndividuo)
import Utils.Outros (shuffle)

-- Retorna a ultima População e o historico de melhores individuos
loopEvolutivoEnumerado :: Ord a => Populacao a -> (Individuo a -> Individuo a) -> (Populacao a -> IO (Populacao a)) -> (Individuo a -> IO (Individuo a)) -> (Individuo a -> Individuo a -> IO (Individuo a, Individuo a)) -> Float -> Int -> IO (GeracaoInfo a)
loopEvolutivoEnumerado _ _ _ _ _ _  0 = return (GeracaoInfo [] [])
loopEvolutivoEnumerado populacao funcaoAvaliacao funcaoSelecao funcaoMutacao funcaoCrossover generatioGap contador = do
    print $ "----------- Loop Evolutivo Enumerado numero " ++ show contador ++ "-----------"

    -- Avaliacao
    let populacaoAvaliada = avaliarPopoulacao populacao

    -- Encontrar o melhor individuo
    let individuoEletista = maybeToList $ melhorIndividuo populacaoAvaliada

    -- Ativando a questão do generatioGap
    (veios, novinhos) <- selecionarQuemFica generatioGap populacaoAvaliada

    -- Selecao
    individuosSelecionados <- funcaoSelecao novinhos

    -- Selecionar pais para fazer crossover
    pais <- selecionarPais individuosSelecionados

    -- Crossover
    novaPopulacao <- fazerCrossover pais

    -- Mutacao
    novaPopulacao' <- mutarPopulacao (concatMap (\(pai, mae) -> [pai, mae]) novaPopulacao)

    -- Ordernar nova interação no Loop evolutivo
    proximaGeracao <- loopEvolutivoEnumerado (individuoEletista ++ novaPopulacao') funcaoAvaliacao funcaoSelecao funcaoMutacao funcaoCrossover generatioGap (contador - 1)

    -- Retornando valores
    return $ GeracaoInfo (individuoEletista ++ veios ++ elitistas proximaGeracao) (calcularMediaFitness populacaoAvaliada : mediaFitness proximaGeracao)


    where
        -- Função auxiliar para calcular a media de fitness de cada geração
        calcularMediaFitness :: Populacao a -> Float
        calcularMediaFitness pop = foldl (\acc (Individuo _ fitness') -> acc + fitness' ) 0.0 pop / fromIntegral (length pop)

        -- Função auxiliar para fazer crossover na população
        -- fazerCrossover :: [(Individuo a, Individuo a)] -> IO [(Individuo a, Individuo a)]
        fazerCrossover pais = sequence $ parMap rpar (uncurry funcaoCrossover) pais

        -- Função auxiliar para calcular o fitness de uma população de forma paralela
        avaliarPopoulacao = parMap rpar funcaoAvaliacao

        -- Função auxliar para realizar a mutação em uma determinada população
        mutarPopulacao pop = sequence (parMap rpar funcaoMutacao pop)

        -- Função auxiliar para selecionar os ficaram e os que morreram de uma determinada população na virada geracional
        selecionarQuemFica :: Float -> Populacao a -> IO (Populacao a, Populacao a)
        selecionarQuemFica 1 pop = return ([], pop)
        selecionarQuemFica gap pop = do
            pop' <- shuffle pop

            return $ splitAt (round $ gap * fromIntegral (length pop)) pop'