{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}

module SAX where


import           Control.Applicative
import           Control.Monad.Fail
import           Data.ByteString hiding (empty)
import           Data.Semigroup hiding (Any)
import           Debug.Trace
import           Prelude hiding (fail, concat)
import           SAX.Streaming
import           Streaming hiding ((<>))
import qualified Streaming.Prelude as S
import           Xeno.Types


data TagState
  = Open ByteString
  | EndOfOpen ByteString
  deriving (Show, Eq, Ord)

type SaxStream = Stream (Of SaxEvent) (Either XenoException) ()

data Result r
  = Partial (SaxEvent -> (Result r)) SaxStream
  -- ^ Supply this continuation with more input so that the parser
  -- can resume.  To indicate that no more input is available, pass
  -- an empty string to the continuation.
  | Done r
  -- ^ The parse succeeded.  The @i@ parameter is the input that had
  -- not yet been consumed (if any) when the parse succeeded.
  | Fail String
  -- ^ The parse failed with current message
  deriving (Functor)

instance Show r => Show (Result r) where
  show (Fail s) = "Fail \"" ++ s ++ "\""
  show (Partial _ _) = "Partial { <...> }"
  show (Done r) = "Done " ++ show r

newtype SaxParser a = SaxParser
  { runSaxParser :: forall r
    . SaxStream
    -> (SaxStream -> Result r)
    -> (SaxStream -> a -> Result r)
    -> Result r
  }

instance Functor SaxParser where
  fmap f (SaxParser p) = SaxParser $ \s fk k ->
    let k' s' a = k s' (f a) in p s fk k'

apm :: SaxParser (a -> b) -> SaxParser a -> SaxParser b
apm pab pa = do
  ab <- pab
  a <- pa
  return $ ab a

instance Applicative SaxParser where
  pure a = SaxParser $ \s _ k -> k s a
  (<*>) = apm
  f *> k = f >>= const k
  k <* f = k >>= \a -> f >> pure a

instance Semigroup (SaxParser a) where
  SaxParser a <> SaxParser b = SaxParser $ \s fk k ->
    let fk' s' = b s' fk k
    in a s fk' k

instance Alternative SaxParser where
  empty = fail "empty alternative"
  (<|>) = (<>)

instance Monad SaxParser where
  return = pure
  SaxParser p >>= k = SaxParser $ \s fk ir ->
    let f s' a = runSaxParser (k a) s' fk ir in p s fk f
  (>>) = (*>)

instance MonadFail SaxParser where
  fail s = SaxParser $ \_ _ _ -> Fail s

parseSax :: SaxParser a -> SaxStream -> Result a
parseSax (SaxParser p) s = p s (\_ -> Fail "fail handler") (\_ a -> Done a)

-- skipUntil :: SaxParser a -> SaxParser a
-- skipUntil (SaxParser p) = SaxParser $ \as st s k ->
--   Partial _ _ _ _

skip :: SaxParser ()
skip = SaxParser $ \s _ k ->
  case S.next s of
    Right (Right (event, s')) ->
      trace ("skip event: " ++ show event) $ k s' ()
    _                         -> Fail "skip: stream exhausted"

openTag :: ByteString -> SaxParser ()
openTag tag = SaxParser $ \s fk k ->
  case S.next s of
   Right (Right (event, s')) ->
     trace ("openTag event: " ++ show event) $
     case event of
       OpenTag tagN -> if tagN == tag then k s' () else fk s
       _            -> fk s
   _                         ->
     trace ("openTag " ++ show tag ++ ": stream exhausted") $
       fk s

endOfOpenTag :: ByteString -> SaxParser ()
endOfOpenTag tag = SaxParser $ \s fk k ->
  case S.next s of
   Right (Right (event, s')) ->
     trace ("endOfOpenTag " ++ show tag ++ " event: " ++ show event) $
     case event of
       EndOfOpenTag tagN -> if tagN == tag then k s' () else fk s
       _                 -> fk s
   _                         ->
     trace ("endOfOpenTag " ++ show tag ++ ": stream exhausted") $
       fk s

text :: SaxParser ByteString
text = SaxParser $ \s fk k -> case S.next s of
  Right (Right (event, s')) ->
    trace ("text event: " ++ show event) $
    -- trace ("stream state: " ++ show s') $
      let
        k' s'' e = case e of
          Text textVal -> k s'' textVal
          _            -> Fail $ "expected a text value, got "
            ++ show e ++ " instead"
      in k' s' event
  _                         ->
     trace ("text: stream exhausted") $ fk s

closeTag :: ByteString -> SaxParser ()
closeTag tag = SaxParser $ \s fk k ->
  case S.next s of
   Right (Right (event, s')) ->
     trace ("closeTag event: " ++ show event) $
     case event of
       CloseTag tagN -> if tagN == tag then k s' () else fk s
       _             -> fk s
   _                         -> Fail "stream exhausted"

withTag :: ByteString -> SaxParser a -> SaxParser a
withTag tag s = do
  openTag tag
  endOfOpenTag tag
  res <- s
  closeTag tag
  pure res

skipUntil :: SaxParser a -> SaxParser a
skipUntil s = s <|> (skip >> skipUntil s)

-- helloParser :: SaxParser Hello
-- helloParser = do
--   withTag "foo" $ do
--     skipUntil $ withTag "hello" $ do
--       hello <- skipUntil $ withTag "inner" text
--       world <- World . concat <$> some (skipUntil $ withTag "world" text)
--       isDom <- (skipUntil $ withTag "is_dom" $ pure True) <|> pure False
--       pure $ Hello hello world isDom

-- skipParser :: SaxParser ByteString
-- skipParser = do
--   skipUntil $ withTag "hello" $ text

-- data World = World ByteString
--   deriving (Show)

-- data Hello = Hello { hHello :: ByteString, hWorld :: World, hIsDom :: Bool }
--   deriving (Show)

-- skipXml :: ByteString
-- skipXml = "<?xml version=\"1.1\"?><foo><nope><nooope><hello>Hello</hello></nooope></nope></foo>"

-- helloXml :: ByteString
-- helloXml = "<?xml version=\"1.1\"?><foo><nope><nooope><hello><inner>Hello</inner><world> wor</world><world>ld!</world></hello></nooope></nope></foo>"
