module Crosssover where


import Tipos (Individuo(genes, Individuo))
import Aleatoriedades (randomInt)


-- A função 'crossoverUmPontoAleatorio' recebe dois indivíduos (pai e mãe) como entrada.
-- Ela retorna um par de novos indivíduos que são o resultado do crossover de um ponto aleatório entre o pai e a mãe.
crossoverUmPontoAleatorio :: Individuo (*) -> Individuo (*) -> IO (Individuo (*), Individuo (*))
crossoverUmPontoAleatorio pai mae 
  -- Verifica se o tamanho dos genes do pai e da mãe são iguais. Se não forem, lança um erro.
  | length (genes pai) /= length (genes mae) = error "O tamanhos dos genes do pai e da mae devem ser iguais"
  -- Verifica se a lista de genes do pai é vazia. Se for, lança um erro.
  | null (genes pai) = error "O numero de genes devem ser maiores que zero"
  -- Se nenhuma das condições acima for verdadeira, realiza o crossover.
  | otherwise = crossover
    where
        -- A função 'crossover' não recebe nenhum argumento.
        -- Ela retorna um par de novos indivíduos que são o resultado do crossover de um ponto aleatório entre o pai e a mãe.
        crossover :: IO (Individuo (*), Individuo (*))
        crossover = do
            -- Gera um número aleatório entre 0 e o tamanho dos genes do pai menos 1.
            numero_aleatorio <- randomInt (0, length (genes pai) - 1)
            -- Divide a lista de genes do pai no ponto aleatório.
            let (pai_1, pai_2) = splitAt numero_aleatorio (genes pai)
            -- Divide a lista de genes da mãe no ponto aleatório.
            let (mae_1, mae_2) = splitAt numero_aleatorio (genes mae)
            -- Retorna um par de novos indivíduos. O primeiro indivíduo tem a primeira parte dos genes da mãe e a segunda parte dos genes do pai.
            -- O segundo indivíduo tem a primeira parte dos genes do pai e a segunda parte dos genes da mãe.
            return (Individuo (mae_1 ++ pai_2) 0, Individuo (pai_1 ++ mae_2) 0)
