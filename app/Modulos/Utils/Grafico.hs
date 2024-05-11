module Utils.Grafico where



import Graphics.Rendering.Chart.Easy
    ( axisGridHide,
      line,
      laxis_override,
      layout_title,
      layout_y_axis,
      plot,
      (.=),
      Default(def) )
import Graphics.Rendering.Chart.Backend.Cairo ( toFile )
import Graphics.Rendering.Chart (laxis_title, layout_x_axis)
import Tipos (GeracaoInfo (GeracaoInfo), Individuo (fitness))

gravarHistorico :: GeracaoInfo a -> String -> IO ()
gravarHistorico (GeracaoInfo historicoMelhorIndividuo mediaPopulacao historicoPiorIndividuo) nomeArquivo =
  toFile def nomeArquivo $ do
    layout_title .= "Historico"
    layout_y_axis . laxis_override .= axisGridHide
    layout_y_axis . laxis_title .= "Valor fitness"
    layout_x_axis . laxis_title .= "Geração"
    plot (line "Melhor individuo" [zip [1 .. length historicoMelhorIndividuo] (extrairDados historicoMelhorIndividuo)])
    plot (line "Media Populacao" [zip [1 .. length mediaPopulacao] mediaPopulacao])
    plot (line "Pior individuo" [zip [1 .. length historicoPiorIndividuo] (extrairDados historicoPiorIndividuo)])

    where
      -- Função auxiliar para extrair os valores de fitness dos individuos elitistas
      extrairDados :: [Individuo a] -> [Float]
      extrairDados = map fitness