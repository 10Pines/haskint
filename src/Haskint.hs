module Haskint where

import Text.Megaparsec
import Data.Void
import Data.Text
import Text.Megaparsec.Char

type Parser = Parsec Void Text

data Expression
  = NumberLiteral Int
  | Identifier String
  | FunctionApplication String Expression
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

functionApplicationParser :: Parser Expression
functionApplicationParser = do
  name <- some letterChar
  space1
  expression <- expressionParser 
  return $ FunctionApplication name expression

expressionParser :: Parser Expression
expressionParser = try functionApplicationParser <|> identifierParser <|> numberLiteralParser

parseExpression :: Text -> Maybe Expression
parseExpression s = parseMaybe expressionParser s
