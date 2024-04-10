module RotinaEvolutiva where


import Mutacao(mutacao)
import Crosssover(umPontoAleatorio, crossover)
import Tipos (GeracaoInfo (GeracaoInfo, elitistas, mediaFitness), Individuo (Individuo), Populacao)
import Avaliacoes.Utils(melhorIndividuo)
import Control.Parallel.Strategies (parMap, rpar)
import Data.Maybe (maybeToList)

-- Retorna a ultima População e o historico de melhores individuos
loopEvolutivoEnumerado :: Ord a => Populacao a -> (Individuo a -> Individuo a) -> (Populacao a -> IO (Populacao a)) -> Float -> Float -> Int -> IO (GeracaoInfo a)
loopEvolutivoEnumerado _ _ _ _ _ 0 = return (GeracaoInfo [] [])
loopEvolutivoEnumerado populacao funcaoAvaliacao funcaoSelecao taxaMutacao probabilidadeCrossover contador = do
    print $ "----------- Loop Evolutivo Enumerado numero " ++ show contador ++ "-----------"

    -- Avaliacao
    let populacaoAvaliada = avaliarPopoulacao populacao

    -- Encontrar o melhor individuo
    let individuoEletista = maybeToList $ melhorIndividuo populacaoAvaliada

    -- Selecao
    individuosSelecionados <- funcaoSelecao populacao

    -- Crossover
    novaPopulacao <- crossover individuosSelecionados umPontoAleatorio probabilidadeCrossover

    -- Mutacao
    novaPopulacao' <- mutarPopulacao novaPopulacao

    -- Ordernar nova interação no Loop evolutivo
    proximaGeracao <- loopEvolutivoEnumerado (individuoEletista ++ novaPopulacao') funcaoAvaliacao funcaoSelecao probabilidadeCrossover taxaMutacao (contador - 1)

    -- Retornando valores
    return $ GeracaoInfo (individuoEletista ++ elitistas proximaGeracao) (calcularMediaFitness populacaoAvaliada : mediaFitness proximaGeracao)


    where
        -- Função auxiliar para calcular a media de fitness de cada geração
        calcularMediaFitness :: Populacao a -> Float
        calcularMediaFitness pop = foldl (\acc (Individuo _ fitness') -> acc + fitness' ) 0.0 pop / fromIntegral (length pop)

        -- Função auxiliar para calcular o fitness de uma população de forma paralela
        avaliarPopoulacao pop = parMap rpar funcaoAvaliacao pop

        -- Função auxliar para realizar a mutação em uma determinada população
        mutarPopulacao :: Populacao a -> IO (Populacao a)
        mutarPopulacao pop = do
            let novaPopulacao = parMap rpar (`mutacao` taxaMutacao) pop
            sequence novaPopulacao