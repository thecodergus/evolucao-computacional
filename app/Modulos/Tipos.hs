module Tipos where

data Individuo a = Individuo
  { genes :: [a],
    fitness :: Float
  } deriving (Show)

type Populacao a = [Individuo a]