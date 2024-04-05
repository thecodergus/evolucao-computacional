module Tipos where


data Individuo a = Individuo
  { genes :: [a],
    fitness :: Float
  } deriving (Show, Eq)

type Populacao a = [Individuo a]
