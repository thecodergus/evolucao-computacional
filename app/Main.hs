module Main where
import Avaliacoes(avaliarFuncaoBool)

main :: IO ()
main = do
    let funcao x = (cos 20 * x) - (abs x / 2) + ((x ** 3) / 4)

    let escopo = (-2, 2)

    let binarios = [False, False, True, False]

    let resultado = avaliarFuncaoBool funcao escopo binarios

    putStrLn $ "Resultado: " ++ show resultado
