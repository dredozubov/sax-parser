{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module SAX
  ( SaxStream
  , Result(..)
  , SaxParser(..)
  , parseSax
  , skipXml
  , skipParser
  , helloXml
  , helloParser
  , Hello(..)
  , skip
  , openTag
  , endOfOpenTag
  , text
  , closeTag
  , streamXml
  ) where


import           Control.Applicative
import           Control.Monad.Fail
import           Data.ByteString hiding (empty)
import           Data.Semigroup hiding (Any)
import           Debug.Tracy
import           Prelude hiding (fail, concat)
import           SAX.Streaming
import           Streaming hiding ((<>))
import qualified Streaming.Prelude as S
import           Xeno.Types


type SaxStream = Stream (Of SaxEvent) (Either XenoException) ()

data Result r
  = Partial (SaxEvent -> (Result r)) [ByteString] SaxStream
  -- ^ Partial result contains a continuation, a list of tags that parser will
  -- skip automatically because `skipUntil` family of combinators were used before
  -- and the rest of the stream to consume
  | Done r
  -- ^ The parse succeeded.  The @i@ parameter is the input that had
  -- not yet been consumed (if any) when the parse succeeded.
  | Fail String
  -- ^ The parse failed with current message
  deriving (Functor)

instance Show r => Show (Result r) where
  show (Fail s) = "Fail \"" ++ s ++ "\""
  show (Partial _ _ _) = "Partial { <...> }"
  show (Done r) = "Done " ++ show r

newtype SaxParser a = SaxParser
  { runSaxParser :: forall r
    . [ByteString]
    -> SaxStream
    -> ([ByteString] -> SaxStream -> Result r)
    -> ([ByteString] -> SaxStream -> a -> Result r)
    -> Result r
  }

instance Functor SaxParser where
  fmap f (SaxParser p) = SaxParser $ \tst s fk k ->
    let k' tst' s' a = k tst' s' (f a) in p tst s fk k'

apm :: SaxParser (a -> b) -> SaxParser a -> SaxParser b
apm pab pa = do
  ab <- pab
  a <- pa
  return $ ab a
{-# INLINE apm #-}

instance Applicative SaxParser where
  pure a = SaxParser $ \tst s _ k -> k tst s a
  {-# INLINE pure #-}

  (<*>) = apm
  {-# INLINE (<*>) #-}

  f *> k = f >>= const k
  {-# INLINE (*>) #-}

  k <* f = k >>= \a -> f >> pure a
  {-# INLINE (<*) #-}

instance Semigroup (SaxParser a) where
  SaxParser a <> SaxParser b = SaxParser $ \tst s fk k ->
    let fk' tst' s' = b tst' s' fk k
    in a tst s fk' k
  {-# INLINE (<>) #-}

instance Alternative SaxParser where
  empty = fail "empty alternative"
  {-# INLINE empty #-}

  (<|>) = (<>)
  {-# INLINE (<|>) #-}

instance Monad SaxParser where
  return = pure
  {-# INLINE return #-}

  SaxParser p >>= k = SaxParser $ \tst s fk ir ->
    let f tst' s' a = runSaxParser (k a) tst' s' fk ir in p tst s fk f
  {-# INLINE (>>=) #-}

  (>>) = (*>)
  {-# INLINE (>>) #-}

instance MonadFail SaxParser where
  fail s = SaxParser $ \_ _ _ _ -> Fail s
  {-# INLINE fail #-}

parseSax :: SaxParser a -> SaxStream -> Result a
parseSax (SaxParser p) s = p [] s (\_ _ -> Fail "fail handler") (\_ _ a -> Done a)
{-# INLINE parseSax #-}

safeHead :: [a] -> Maybe (a, [a])
safeHead [] = Nothing
safeHead (a:as) = Just (a, as)
{-# INLINE safeHead #-}

skip :: SaxParser ()
skip = SaxParser $ \tst s _ k ->
  case S.next s of
    Right (Right (_, s')) -> k tst s' ()
    _                       -> Fail "skip: stream exhausted"
{-# INLINE skip #-}

-- skipAttributes :: SaxParser ()
-- skipAttributes = SaxParser $ \tst s _ k ->
--   case S.next s of
--     Right (Right (event, s')) ->
--       case event of
--         Attr _ _ -> k tst s' ()
--         _        ->

skipAndMark :: SaxParser ()
skipAndMark = SaxParser $ \tst s _ k ->
  case S.next s of
    Right (Right (event, s')) ->
      tracy ("skipAndMark event: " ++ show event) $
      case event of
        EndOfOpenTag tag ->
          tracy ("adding to the skip stack: " ++ show tag) $
          k (tag:tst) s' ()
        _                -> k tst s' ()
    _                         -> Fail "skip: stream exhausted"
{-# INLINE skipAndMark #-}

openTag :: ByteString -> SaxParser ()
openTag tag = SaxParser $ \tst s fk k ->
  case S.next s of
    Right (Right (event, s')) ->
      tracy ("openTag " ++ show tag ++ " event: " ++ show event) $
      case event of
        OpenTag tagN -> if tagN == tag then k tst s' () else fk tst s
        e            -> case safeHead tst of
          Nothing -> fk tst s
          Just (tagS,rest) -> if e == CloseTag tagS
            then k rest s' ()
            else fk tst s
    Right (Left e)            -> Fail (show e)
    Left _                    -> Fail "SAX stream exhausted"
{-# INLINE openTag #-}

endOfOpenTag :: ByteString -> SaxParser ()
endOfOpenTag tag = SaxParser $ \tst s fk k ->
  case S.next s of
   Right (Right (event, s')) ->
     tracy ("endOfOpenTag " ++ show tag ++ " event: " ++ show event) $
     case event of
       EndOfOpenTag tagN -> if tagN == tag then k tst s' () else fk tst s
       e            -> case safeHead tst of
         Nothing -> fk tst s
         Just (tagS,rest) -> if e == CloseTag tagS
           then k rest s' ()
           else Fail $ "expected end of opening of " ++ show tag ++ ", got "
             ++ show event ++ " instead"
   Right (Left e)            -> Fail (show e)
   Left _                    -> Fail "SAX stream exhausted"
{-# INLINE endOfOpenTag #-}

text :: SaxParser ByteString
text = SaxParser $ \tst s fk k -> case S.next s of
  Right (Right (event, s')) ->
    tracy ("text event: " ++ show event) $
    tracy ("tst: " ++ show tst) $
      let
        k' tst' s'' a = case event of
          Text textVal -> k tst' s'' textVal
          e            -> case safeHead tst of
            Nothing -> fk tst s
            Just (tagS,rest) -> if e == CloseTag tagS
              then k rest s' a
              else fk tst s'
      in k' tst s' ""
  Right (Left e)            -> Fail (show e)
  Left _                    -> Fail "SAX stream exhausted"
{-# INLINE text #-}

closeTag :: ByteString -> SaxParser ()
closeTag tag = SaxParser $ \tst s fk k ->
  case S.next s of
   Right (Right (event, s')) ->
     tracy ("closeTag " ++ show tag ++ " event: " ++ show event) $
     case event of
       CloseTag tagN -> tracy "close1" $ if tagN == tag then k tst s' ()
         else case safeHead tst of
           Nothing -> fk tst s
           Just (tagS,rest) -> if tagS == tagN then k rest s' () else k tst s' ()
       e             -> fk tst s'
   Right (Left e)            -> Fail (show e)
   Left _                    -> Fail "SAX stream exhausted"
{-# INLINE closeTag #-}

withTag :: ByteString -> SaxParser a -> SaxParser a
withTag tag s = do
  openTag tag
  endOfOpenTag tag
  res <- s
  closeTag tag
  pure res
{-# INLINE withTag #-}

withTag' :: ByteString -> SaxParser a -> SaxParser a
withTag' t p = skipUntil (withTag t p)
{-# INLINE withTag' #-}

skipUntil :: SaxParser a -> SaxParser a
skipUntil s = s <|> (skipAndMark >> skipUntil s)
{-# INLINE skipUntil #-}

-- tests
helloParser :: SaxParser Hello
helloParser = do
  skipUntil $ withTag "foo" $ do
    skipUntil $ withTag "hello" $ do
      hello <- withTag "inner" text
      world <- World . concat <$> some (withTag "world" text)
      isDom <- (withTag "is_dom" $ pure True) <|> pure False
      pure $ Hello hello world isDom

skipParser :: SaxParser ByteString
skipParser = do
  skipUntil $ withTag "hello" $ text

newtype World = World ByteString deriving (Show)
data Hello = Hello { hHello :: ByteString, hWorld :: World, hIsDom :: Bool } deriving (Show)

skipXml :: ByteString
skipXml = "<?xml version=\"1.1\"?><foo><nope><nooope><hello>Hello</hello></nooope></nope></foo>"

helloXml :: ByteString
helloXml = "<?xml version=\"1.1\"?><f><foo><bar><hello><inner>Hello</inner><world> wor</world><world>ld!</world></hello></bar></foo></f>"
