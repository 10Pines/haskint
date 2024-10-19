module Haskint where

import Text.Megaparsec
import Data.Void
import Data.Text
import Text.Megaparsec.Char

type Parser = Parsec Void Text

data Expression
  = NumberLiteral Int
  | Identifier String
  deriving (Show, Eq)

numberLiteralParser :: Parser Expression
numberLiteralParser = do
  numberDigits <- some digitChar
  let numberValue = read numberDigits
  return $ NumberLiteral numberValue

identifierParser :: Parser Expression
identifierParser = do
  name <- some letterChar
  return $ Identifier name

expressionParser :: Parser Expression
expressionParser = identifierParser <|> numberLiteralParser

parseExpression :: Text -> Maybe Expression
parseExpression s = parseMaybe expressionParser s
