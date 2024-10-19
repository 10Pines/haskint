module Haskint where

import Text.Megaparsec
import Data.Void
import Data.Text
import Text.Megaparsec.Char

type Parser = Parsec Void Text

data Expression
  = NumberLiteral Int
  deriving (Show, Eq)


parseNumberLiteral :: Parser Expression
parseNumberLiteral = do
  n <- many digitChar
  let number = read n
  return $ NumberLiteral number


parseExpression :: Text -> Maybe Expression
parseExpression s =
  parseMaybe (parseNumberLiteral) s
