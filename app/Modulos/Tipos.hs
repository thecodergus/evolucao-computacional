module Tipos where


data Individuo
  = IndividuoFlutuante [Float]
  | IndividuoInteiro [Int]
  | IndividuoBooleano [Bool]
  deriving (Show)
