{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Applicative
import Data.ByteString as BS
import Data.Foldable
import Data.Monoid
import SAX
import Test.Hspec


newtype World = World ByteString deriving (Show, Eq)
data Hello = Hello
  { hHello       :: ByteString
  , hWorld       :: World
  , hIsDom       :: Bool
  , hNonExistent :: [ByteString]
  } deriving (Show, Eq)

newtype R r = R (Result r)

instance Eq r => Eq (R r) where
  (R (Done a)) == (R (Done b)) = a == b
  (R (Fail a)) == (R (Fail b)) = a == b
  _            == _            = False

helloXml :: ByteString
helloXml = "<?xml version=\"1.1\"?><f><foo bla=\"alb\"><bar><hello><inner>Hello</inner><skipMe><meToo>and Me</meToo></skipMe><world> wor</world><world>ld!</world></hello></bar></foo><quuz>ok</quuz></f>"

helloParser :: SaxParser Hello
helloParser = do
  withTag "f" $ do
    withTag "foo" $ do
      withTag "bar" $ do
        withTag "hello" $ do
          hello <- withTag "inner" bytes
          skipTag "skipMe"
          world <- World . BS.concat <$> some (withTag "world" bytes)
          isDom <- (withTag "is_dom" $ pure True) <|> pure False
          ne <- many (withTag "fish" bytes)
          pure $ Hello hello world isDom ne

skipTagXmls :: [(ByteString, SaxParser ByteString)]
skipTagXmls = fmap (\(x,p) -> ("<?xml version=\"1.1\"?>" <> x, p))
  [ ("<a></a><b>b</b>", skipTag "a" >> withTag "b" bytes)
  , ("<a><nested></nested></a><b>b</b>", skipTag "a" >> withTag "b" bytes)
  ]

atTagXmls :: [(ByteString, SaxParser ByteString, R ByteString)]
atTagXmls = fmap (\(x,p, r) -> ("<?xml version=\"1.1\"?>" <> x, p, r))
  [ ("<b>b</b>", atTag "b" bytes, R (Done "b"))
  , ("<a><b>b</b></a>", atTag "a" $ atTag "b" bytes, R (Done "b"))
  , ("<a><c cattr=\"c\">c</c><b>b</b></a>", atTag "a" $ atTag "b" bytes, R (Done "b"))
  , ("<a></a>", atTag "b" bytes, R (Fail "no tag b found"))
  , ("<a><c cattr=\"c\">c</c></a><b>b</b><c>true</c>", atTag "a" $ atTag "b" bytes, R (Done "true"))
  ]

main :: IO ()
main = hspec $ do
  describe "parser" $ do
    it "works" $ do
      parseSax helloParser (streamXml helloXml) `shouldSatisfy`
        \res -> case res of
          (Done r ) -> r == Hello "Hello" (World " world!") False []
          _         -> False
    describe "skipTag" $ do
      for_ skipTagXmls $ \(xml, parser) ->
        it (" parses " ++ show xml) $ do
          parseSax parser (streamXml xml) `shouldSatisfy` \case
            Done _ -> True
            _      -> False

    describe "atTag" $ do
      for_ atTagXmls $ \(xml, parser, result) ->
        it (" parses " ++ show xml) $ do
          parseSax parser (streamXml xml) `shouldSatisfy` ((==result) . R)
