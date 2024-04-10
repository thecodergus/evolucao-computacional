module Tipos where


data Individuo a = Individuo
  { genes :: [a],
    fitness :: Float
  } deriving (Show, Eq)

type Populacao a = [Individuo a]


data GeracaoInfo a = GeracaoInfo {
  elitistas :: [Individuo a],
  mediaFitness :: [Float]
} deriving Show