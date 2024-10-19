module Haskint where

import Text.Megaparsec
import Data.Void
import Data.Text
import Text.Megaparsec.Char

type Parser = Parsec Void Text

data Expression
  = NumberLiteral Int
  | Identifier String
  | FunctionApplication Expression Expression
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
  name <- identifierParser
  space1
  expression <- expressionParser
  return $ FunctionApplication name expression

parenthesisParser :: Parser Expression
parenthesisParser = do
  _ <- char '('
  expression <- expressionParser
  _ <- char ')'
  return expression
  
expressionParser :: Parser Expression
expressionParser = try functionApplicationParser <|> parenthesisParser <|> identifierParser <|> numberLiteralParser

parseExpression :: Text -> Maybe Expression
parseExpression = parseMaybe expressionParser
