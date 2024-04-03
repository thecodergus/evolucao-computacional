module Avaliacoes.Matematica where

-- Função f(x)


boolListToDecimal :: [Bool] -> Float
boolListToDecimal bools = fromIntegral (boolToInt (take 4 bools)) + boolToFloat (drop 4 bools)
   where
      boolToInt :: [Bool] -> Int
      boolToInt lista = foldl (\acc b -> acc * 2 + fromEnum b) 0  (reverse lista)

      boolToFloat :: [Bool] -> Float
      boolToFloat lista = foldr (\b acc -> (acc + fromIntegral (fromEnum b)) / 2) 0.0 lista

-- Fitness para maximização (usando f(x))
fitnessMax :: (Float -> Float) -> [Bool] -> Float
fitnessMax f bools = f (boolListToDecimal bools)

-- Fitness para minimização (usando -f(x))
fitnessMin :: (Float -> Float) -> [Bool] -> Float
fitnessMin f bools = - f (boolListToDecimal bools)