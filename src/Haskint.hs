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
  firstExpression <- expressionInsideApplicationParser
  restOfExpressions <- some (try $ space1 >> expressionInsideApplicationParser)
  return $ Prelude.foldl1 FunctionApplication $ firstExpression : restOfExpressions

parenthesisParser :: Parser Expression
parenthesisParser = do
  _ <- char '('
  expression <- expressionParser
  _ <- char ')'
  return expression

expressionInsideApplicationParser :: Parser Expression
expressionInsideApplicationParser = parenthesisParser <|> identifierParser <|> numberLiteralParser

infixOperatorParser :: Parser Expression
infixOperatorParser = do
  leftExpression <- infixOperatorArgumentParser
  operator <- space1 >> char '+' >> space1
  rightExpression <- infixOperatorArgumentParser
  return $ FunctionApplication
    (FunctionApplication (Identifier "+") leftExpression)
    rightExpression
  
infixOperatorArgumentParser :: Parser Expression
infixOperatorArgumentParser = try functionApplicationParser <|> expressionInsideApplicationParser

expressionParser :: Parser Expression
expressionParser = try infixOperatorParser <|> infixOperatorArgumentParser

parseExpression :: Text -> Maybe Expression
parseExpression = parseMaybe expressionParser
