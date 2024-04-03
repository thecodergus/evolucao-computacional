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
      boolToFloat lista = foldr lambda 0.0 lista
         where
            lambda b acc = (acc + fromIntegral (fromEnum b)) / 2

-- Fitness para maximização (usando f(x))
fitnessMax :: (Float -> Float) -> [Bool] -> Float
fitnessMax f bools = f (boolListToDecimal bools)

-- Fitness para minimização (usando -f(x))
fitnessMin :: (Float -> Float) -> [Bool] -> Float
fitnessMin f bools = - f (boolListToDecimal bools)