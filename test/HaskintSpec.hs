{-# LANGUAGE OverloadedStrings #-}
module HaskintSpec (main, spec) where

import Test.Hspec

import Haskint
import Haskint (Expression(Identifier, FunctionApplication))

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "parse" $ do
    it "cannot parse invalid syntax" $ do
      parseExpression "1aa2" `shouldBe` Nothing

    it "cannot parse an empty string" $ do
      parseExpression "" `shouldBe` Nothing

    it "parses a number literal" $ do
      parseExpression "12" `shouldBe` Just (NumberLiteral 12)

    it "parses variables" $ do
      parseExpression "lala" `shouldBe` Just (Identifier "lala")

    it "parses function application of a number literal" $ do
      parseExpression "f 1" `shouldBe` Just (application "f" (NumberLiteral 1))

    it "parses function application of an identifier" $ do
      parseExpression "f a" `shouldBe` Just (application "f" (Identifier "a"))

    it "parses parenthesis" $ do
      parseExpression "(1)" `shouldBe` Just (NumberLiteral 1)
    
    it "parses parenthesis in the middle" $ do
      parseExpression "f (1)" `shouldBe` Just (application "f" (NumberLiteral 1))
    
    it "parses parenthesis double" $ do
      parseExpression "f ((1))" `shouldBe` Just (application "f" (NumberLiteral 1))
    
    it "cannot parse empty parenthesis" $ do
      parseExpression "()" `shouldBe` Nothing

    it "parses applications of applications" $ do
      parseExpression "f a b" `shouldBe` Just (
          FunctionApplication
            (application "f" (Identifier "a"))
            (Identifier "b")
        )

    it "parses sums" $ do
      -- (+) (f x) (f y)
      parseExpression "f x + f y" `shouldBe` Just (
          FunctionApplication
            (FunctionApplication
              (Identifier "+")
              (application "f" $ Identifier "x"))
            (application "f" $ Identifier "y")
        )

    where
      application :: String -> Expression -> Expression
      application functionName argument = FunctionApplication (Identifier functionName) argument

