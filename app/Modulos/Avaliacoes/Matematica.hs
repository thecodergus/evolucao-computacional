module Avaliacoes.Matematica where

-- Converte uma lista de booleanos para um valor decimal após a vírgula.
boolListToDecimal :: [Bool] -> Float
boolListToDecimal bools = fromIntegral (boolToInt (take 4 bools)) + boolToFloat (drop 4 bools)
   where
      -- Converte uma lista de booleanos para um valor inteiro.
      boolToInt :: [Bool] -> Int
      boolToInt lista = foldl lambda 0 (reverse lista)
         where
            lambda acc b = acc * 2 + fromEnum b

      -- Converte uma lista de booleanos para um valor de ponto flutuante.
      boolToFloat :: [Bool] -> Float
      boolToFloat = foldr lambda 0.0 where
            lambda b acc = (acc + fromIntegral (fromEnum b)) / 2

-- f :: Float -> Float
-- f x = cos (20 * x) - abs x / 2 + x ** 3 / 4

-- Fitness para maximização (usando f(x))
fitnessMax :: (Float, Float) -> (Float -> Float) -> [Bool] -> Float
fitnessMax (domA, _) f bools
      | domA <= 0 = f (boolListToDecimal bools) + (abs domA + 0.01)
      | otherwise = f (boolListToDecimal bools)

-- Fitness para minimização (usando -f(x))
fitnessMin :: (Float, Float) -> (Float -> Float) -> [Bool] -> Float
fitnessMin (domA, _) f bools 
      | domA <= 0 = 1 / (f (boolListToDecimal bools) + (abs domA + 0.01))
      | otherwise = 1 / (f (boolListToDecimal bools) + 1)