module Grafico where



import Graphics.Rendering.Chart.Easy
import Tipos (Populacao)
import Graphics.Rendering.Chart.Backend.Cairo


gravarHistorico :: [Float] -> IO String
gravarHistorico historico = 
    toFile def "grafico.png" $ do
        layoutlr_title .= "Grafico"
        layoutlr_left_axis . laxis_override .= axisGridHide
        layoutlr_right_axis . laxis_override .= axisGridHide
        plotLeft (line "Melhor Individuo" [[(d, v) | (d, v) <- evolucao]])

        where
            evolucao :: [Float] -> [(Int, Float)]
            evolucao xs = zip [1..] xs

