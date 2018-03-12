{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

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
{-# INLINE apm #-}

instance Applicative SaxParser where
  pure a = SaxParser $ \s _ k -> k s a
  {-# INLINE pure #-}

  (<*>) = apm
  {-# INLINE (<*>) #-}

  f *> k = f >>= const k
  {-# INLINE (*>) #-}

  k <* f = k >>= \a -> f >> pure a
  {-# INLINE (<*) #-}

instance Semigroup (SaxParser a) where
  SaxParser a <> SaxParser b = SaxParser $ \s fk k ->
    let fk' s' = b s' fk k
    in a s fk' k
  {-# INLINE (<>) #-}

instance Alternative SaxParser where
  empty = fail "empty alternative"
  {-# INLINE empty #-}

  (<|>) = (<>)
  {-# INLINE (<|>) #-}

instance Monad SaxParser where
  return = pure
  {-# INLINE return #-}

  SaxParser p >>= k = SaxParser $ \s fk ir ->
    let f s' a = runSaxParser (k a) s' fk ir in p s fk f
  {-# INLINE (>>=) #-}

  (>>) = (*>)
  {-# INLINE (>>) #-}

instance MonadFail SaxParser where
  fail s = SaxParser $ \_ _ _ -> Fail s
  {-# INLINE fail #-}

parseSax :: SaxParser a -> SaxStream -> Result a
parseSax (SaxParser p) s = p s (\_ -> Fail "fail handler") (\_ a -> Done a)
{-# INLINE parseSax #-}

skip :: SaxParser ()
skip = SaxParser $ \s _ k ->
  case S.next s of
    Right (Right (event, s')) ->
      trace ("skip event: " ++ show event) $ k s' ()
    _                         -> Fail "skip: stream exhausted"
{-# INLINE skip #-}

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
{-# INLINE openTag #-}

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
{-# INLINE endOfOpenTag #-}

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
{-# INLINE text #-}

closeTag :: ByteString -> SaxParser ()
closeTag tag = SaxParser $ \s fk k ->
  case S.next s of
   Right (Right (event, s')) ->
     trace ("closeTag event: " ++ show event) $
     case event of
       CloseTag tagN -> if tagN == tag then k s' () else fk s
       _             -> fk s
   _                         -> Fail "stream exhausted"
{-# INLINE closeTag #-}

withTag :: ByteString -> SaxParser a -> SaxParser a
withTag tag s = do
  openTag tag
  endOfOpenTag tag
  res <- s
  closeTag tag
  pure res
{-# INLINE withTag #-}

skipUntil :: SaxParser a -> SaxParser a
skipUntil s = s <|> (skip >> skipUntil s)
{-# INLINE skipUntil #-}
