module Tipos where


data Individuo
  = IndividuoFlutuante [Float]
  | IndividuoInteiroBound [Int]
  | IndividuoInteiroPermutado [[Int]]
  | IndividuoBooleano [Bool]
  deriving (Show)

type Populacao = [Individuo]