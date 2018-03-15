{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Applicative
import Data.ByteString as BS
import SAX
import Test.Hspec


newtype World = World ByteString deriving (Show, Eq)
data Hello = Hello
  { hHello :: ByteString
  , hWorld :: World
  , hIsDom :: Bool
  , hNonExistent :: [ByteString]
  } deriving (Show, Eq)

helloXml :: ByteString
helloXml = "<?xml version=\"1.1\"?><f><foo bla=\"alb\"><bar><hello><inner>Hello</inner><skipMe><meToo>and Me</meToo></skipMe><world> wor</world><world>ld!</world></hello></bar></foo></f>"

helloParser :: SaxParser Hello
helloParser = do
  atTag "foo" $ do
    atTag "hello" $ do
      hello <- withTag "inner" bytes
      skipTag "skipMe"
      world <- World . BS.concat <$> some (withTag "world" bytes)
      isDom <- (withTag "is_dom" $ pure True) <|> pure False
      ne <- many (withTag "fish" bytes) <|> pure []
      pure $ Hello hello world isDom ne

main :: IO ()
main = hspec $ do
  describe "parser" $ do
    it "works" $ do
      parseSax helloParser (streamXml helloXml) `shouldSatisfy`
        \res -> case res of
          (Done r ) -> r == Hello "Hello" (World " world!") False []
          _         -> False
