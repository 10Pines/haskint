{-# LANGUAGE OverloadedStrings #-}
module HaskintSpec (main, spec) where

import Test.Hspec
import Test.QuickCheck

import Haskint

-- `main` is here so that this module can be run from GHCi on its own.  It is
-- not needed for automatic spec discovery.
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
      parseExpression "f 1" `shouldBe` Just (FunctionApplication "f" (NumberLiteral 1))

    it "parses function application of an identifier" $ do
      parseExpression "f a" `shouldBe` Just (FunctionApplication "f" (Identifier "a"))


    -- it "is idempotent" $ property $
    --   \str -> strip str === strip (strip str)
