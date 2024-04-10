module Grafico where



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


gravarHistorico :: [Float] -> String -> IO ()
gravarHistorico historico nomeArquivo =
  toFile def nomeArquivo $ do
    layout_title .= "Historico"
    layout_y_axis . laxis_override .= axisGridHide
    layout_y_axis . laxis_title .= "Melhor individuo (Valor fitness)"
    layout_x_axis . laxis_title .= "Geração"
    plot (line "Melhor individuo" [zip [1 .. length historico] historico])
