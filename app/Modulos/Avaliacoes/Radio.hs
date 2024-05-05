module Radio where
import Tipos (Individuo (Individuo))


avaliacao :: Individuo Bool -> Individuo Bool
avaliacao (Individuo gene _)
    | length gene /= 10 = error "O gene deve ter 10 bits obrigatoriamente!"
    | otherwise = do
    
    let (st, lx) = splitAt 5 gene

    let st' = boolToFloat st / divSt
    let lx' = boolToFloat lx / divLx


    Individuo gene (fitness st' lx')

    where
        divSt :: Float
        divSt = 24 / 5

        divLx :: Float
        divLx = 16 / 5

        boolToFloat :: [Bool] -> Float
        boolToFloat = foldr (\x acc -> (if x then 1 else 0) + (-2)*acc) 0

        funcaoObjetivo :: Float -> Float -> Float
        funcaoObjetivo st lx = 30 * st + 40 * lx

        restricao :: Float -> Float -> Bool
        restricao st lx  = st + 2 * lx <= 40 && 0 <= st && st <= 24 && 0 <= lx && lx <= 16

        fitness :: Float -> Float -> Float
        fitness st lx
            | restricao st lx = foN + r * hN
            | otherwise = 0
            where
                r :: Float
                r = -1

                foN :: Float
                foN = funcaoObjetivo st lx / 1360

                hN :: Float
                hN = maximum [0..((st + 2 * lx - 40) / 16)]



         