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
  expressions <- sepBy1 expressionInsideApplicationParser space1
  return $ Prelude.foldl1 FunctionApplication expressions

parenthesisParser :: Parser Expression
parenthesisParser = do
  _ <- char '('
  expression <- expressionParser
  _ <- char ')'
  return expression

expressionInsideApplicationParser :: Parser Expression
expressionInsideApplicationParser = parenthesisParser <|> identifierParser <|> numberLiteralParser
  
expressionParser :: Parser Expression
expressionParser = try functionApplicationParser <|> expressionInsideApplicationParser

parseExpression :: Text -> Maybe Expression
parseExpression = parseMaybe expressionParser
