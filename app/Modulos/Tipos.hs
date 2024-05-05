{-# LANGUAGE InstanceSigs #-}
module Tipos where


data Individuo a = Individuo
  { genes :: [a],
    fitness :: Float
  } deriving (Show)

instance Ord a => Ord (Individuo a) where
  compare :: Ord a => Individuo a -> Individuo a -> Ordering
  compare i1 i2 = compare (fitness i1) (fitness i2)

instance (Eq a) => Eq (Individuo a) where
  (==) :: Eq a => Individuo a -> Individuo a -> Bool
  (Individuo g1 _) == (Individuo g2 _) = g1 == g2
  (/=) :: Eq a => Individuo a -> Individuo a -> Bool
  (Individuo g1 _) /= (Individuo g2 _) = g1 /= g2

type Populacao a = [Individuo a]


data GeracaoInfo a = GeracaoInfo {
  elitistas :: [Individuo a],
  mediaFitness :: [Float]
} deriving Show