{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Applicative as A
import Debug.Tracy
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
helloXml = "<?xml version=\"1.1\"?><f><foo bla=\"alb\"><bar><hello><inner>Hello</inner><skipMe><meToo>and Me</meToo></skipMe><world> wor</world><world>ld!</world></hello></bar></foo></f>"

helloParser :: SaxParser Hello
helloParser = do
  withTags ["f", "foo", "bar", "hello"] $ do
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

attrXmls :: [(ByteString, SaxParser ByteString, R ByteString)]
attrXmls = fmap (\(x,p, r) -> ("<?xml version=\"1.1\"?>" <> x, p, r))
  [ ("<a b=\"b\"> </a>", withAttrs "a" (attr "b"), R $ Done "b")
  ]

skipAttrXmls :: [(ByteString, SaxParser (), R ())]
skipAttrXmls = fmap (\(x,p, r) -> ("<?xml version=\"1.1\"?>" <> x, p, r))
  [ ("<a b=\"b\" c=\"c\"></a>", withAttrs "a" (many skipAttr *> pure ()), R $ Done ())
  ]

anyAttrXmls :: [(ByteString, SaxParser [(ByteString, ByteString)], R [(ByteString, ByteString)])]
anyAttrXmls = fmap (\(x,p, r) -> ("<?xml version=\"1.1\"?>" <> x, p, r))
  [ ("<a b=\"b\" c=\"c\"></a>", withAttrs "a" (some anyAttr), R (Done [("b","b"), ("c", "c")]))
  ]

namespaceXmls :: [(ByteString, SaxParser (), R ())]
namespaceXmls = fmap (\(x,p, r) -> ("<?xml version=\"1.1\"?>" <> x, p, r))
  [ ("<test:a></a>", openTag "a", R (Fail "fail handler"))
  , ("<test:a></a>", openTag "test:a", R (Done ()))
  , ("<test:a></a>", openTag' (AnyNS "a"), R (Done ()))
  , ("<a></a>", openTag' (AnyNS "a"), R (Done ()))
  , ("<test:a></test:a>", withTag' (AnyNS "a") (pure ()), R (Done ()))
  ]

atTagXmls :: [(ByteString, SaxParser ByteString, R ByteString)]
atTagXmls = fmap (\(x,p, r) -> ("<?xml version=\"1.1\"?>" <> x, p, r))
  [ ("<b>b</b>", atTag "b" bytes, R (Done "b"))
  , ("<a><b>b</b></a>", atTag "a" $ atTag "b" bytes, R (Done "b"))
  , ("<a><c cattr=\"c\">c</c><b>b</b></a>", atTag "a" $ atTag "b" bytes, R (Done "b"))
  -- , ("<a></a>", atTag "b" bytes, R (Fail "()"))
  , ("<a><a></a></a><b>b</b>", atTag "b" bytes, R (Done "b"))
  , ("<a><c cattr=\"c\">c</c></a><b>b</b><c>true</c>", atTag "a" $ atTag "c" bytes, R (Done "c"))
  , ("<a><c cattr=\"c\">c</c></a><b>b</b><c>true</c>", atTag "c" bytes, R (Done "true"))
  , ("<test:a></test:a><test:b></test:b>", atTag' (AnyNS "b") (pure "ok"), R (Done "ok"))
  , ("<a>a</a><b>b</b>", atTag "a" bytes >> withTag "b" bytes, R (Done "b"))
  , ("<a>a</a><a>a</a><b>b</b>", some (withTag "a" bytes) >> withTag "b" bytes, R (Done "b"))
  , ("<o>o</o><u>u</u><a>a</a><a>a</a><b>b</b>", BS.concat <$> some (atTag "a" bytes), R (Done "aa"))
  -- , ("<o>o</o><u>u</u><a>a</a><a>a</a><b>b</b>", some (atTag "a" bytes) >> withTag "b" bytes, R (Done "b"))
  -- , ("<o>o</o><u>u</u><a>a</a><a><b>a</b></a><b>b</b>", many (atTag "a" bytes) >> withTag "b" bytes, R (Done "b"))
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

    describe "attr" $ do
      for_ attrXmls $ \(xml, parser, result) ->
        it (" parses " ++ show xml) $ do
          parseSax parser (streamXml xml) `shouldSatisfy` ((==result) . R)

    describe "anyAttr" $ do
      for_ anyAttrXmls $ \(xml, parser, result) ->
        it (" parses " ++ show xml) $ do
          parseSax parser (streamXml xml) `shouldSatisfy` ((==result) . R)

    describe "skipAttr" $ do
      for_ skipAttrXmls $ \(xml, parser, result) ->
        it (" parses " ++ show xml) $ do
          parseSax parser (streamXml xml) `shouldSatisfy` ((==result) . R)

    describe "openTag" $ do
      for_ namespaceXmls $ \(xml, parser, result) ->
        it (" parses " ++ show xml) $ do
          parseSax parser (streamXml xml) `shouldSatisfy` ((==result) . R)
