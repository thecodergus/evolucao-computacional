module Complexidade.Tipos where

data Trajeto = Trajeto {de :: String, para :: String, distancia :: Maybe Integer} deriving(Eq, Ord)

instance Show Trajeto where
    show (Trajeto de' para' distancia') = "(" ++ de' ++ ") -> (" ++ para' ++ ") = " ++ show distancia'
