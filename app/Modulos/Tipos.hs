module Tipos where

data Intervalo a = Intervalo {minimo :: a, maximo :: a} deriving (Show)

data Individuo a = Individuo
  { genes :: [a],
    tamanho :: Int,
    intervalo :: Intervalo a
  } deriving (Show)

type Populacao a = [Individuo a]