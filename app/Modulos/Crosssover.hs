module Crosssover where


import Tipos (Individuo(genes, Individuo))
import GerarAleatoriedades (randomInt)


crossoverUmPontoAleatorio :: Individuo (*) -> Individuo (*) -> IO (Individuo (*), Individuo (*))
crossoverUmPontoAleatorio pai mae | length (genes pai) /= length (genes mae) = error "O tamanhos dos genes do pai e da mae devem ser iguais"
                         | null (genes pai) = error "O numero de genes devem ser maiores que zero"
                         | otherwise = crossover
    where
        crossover :: IO (Individuo (*), Individuo (*))
        crossover = do
            numero_aleatorio <- randomInt (1, length (genes pai))
            let (pai_1, pai_2) = splitAt numero_aleatorio (genes pai)
            let (mae_1, mae_2) = splitAt numero_aleatorio (genes mae)


            return (Individuo (mae_1 ++ pai_2) 0, Individuo (pai_1 ++ mae_2) 0)