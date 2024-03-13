{-# OPTIONS_GHC -Wno-name-shadowing #-}
module Avaliacoes where

import Tipos (Individuo(fitness))

-- Função auxiliar para encontrar o melhor individuo com base no fitness
maiorFitness :: Ord a => [Individuo a] -> Individuo a
maiorFitness [] = 
    error "Lista vazia"
maiorFitness (x : xs) = 
    foldl1 (\acc x -> if fitness x > fitness acc then x else acc) (x : xs)
