{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Applicative
import Data.ByteString as BS
import SAX
import Test.Hspec


newtype World = World ByteString deriving (Show, Eq)
data Hello = Hello { hHello :: ByteString, hWorld :: World, hIsDom :: Bool } deriving (Show, Eq)

helloXml :: ByteString
helloXml = "<?xml version=\"1.1\"?><f><foo><bar><hello><inner>Hello</inner><world> wor</world><world>ld!</world></hello></bar></foo></f>"

helloParser :: SaxParser Hello
helloParser = do
  withTag' "foo" $ do
    withTag' "hello" $ do
      hello <- withTag "inner" bytes
      world <- World . BS.concat <$> some (withTag "world" bytes)
      isDom <- (withTag "is_dom" $ pure True) <|> pure False
      pure $ Hello hello world isDom

main :: IO ()
main = hspec $ do
  describe "parser" $ do
    it "works" $ do
      parseSax helloParser (streamXml helloXml) `shouldSatisfy`
        \res -> case res of
          (Done r ) -> r == Hello "Hello" (World " world!") False
          _         -> False
