module Complexidade.Parser where

import Text.Parsec
    ( anyChar, char, digit, string, eof, manyTill, parse, try, Parsec, (<|>))
import Complexidade.Tipos
import Debug.Trace


parserTrajeto :: Parsec String () Trajeto
parserTrajeto = do
    _ <- char '('
    de' <- manyTill anyChar (try (string ") -> ("))
    para' <- manyTill anyChar (try (string ") = "))
    Trajeto de' para' <$> parserMetros

parserMetros :: Parsec String () (Maybe Int)
parserMetros =
  try (Just . read <$> digit `manyTill` try (char '\n'))
    <|> (anyChar `manyTill` try (char '\n') >> return Nothing)

parserTrajetos :: Parsec String () [Trajeto]
parserTrajetos = manyTill parserTrajeto eof

parser :: String -> [Trajeto]
parser input =
    case parse parserTrajetos "" input of
        Left _ -> []
        Right x -> x