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
  , skip
  , skipAndMark
  , openTag
  , endOfOpenTag
  , bytes
  , closeTag
  , skipUntil
  , withTag
  , skipTag
  , atTag
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
  -- skip automatically(if parsers from `skipUntil` family of combinators were used before)
  -- the rest of the stream to consume
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
    Right (Right (e, s')) ->
      tracy ("skipping event: " ++ show e) $
      k tst s' ()
    _                       -> Fail "skip: stream exhausted"
{-# INLINE skip #-}

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
       e                 -> case safeHead tst of
         Nothing          ->
           Fail $ "expected an ending of tag opening of "
           ++ show tag ++ ", got " ++ show event ++ " instead"
         Just (tagS,rest) -> if e == CloseTag tagS
           then k rest s' ()
           else fk tst s
   Right (Left e)            -> Fail (show e)
   Left _                    -> Fail "SAX stream exhausted"
{-# INLINE endOfOpenTag #-}

bytes :: SaxParser ByteString
bytes = SaxParser $ \tst s fk k -> case S.next s of
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
              else Fail $ "expected text value, got "
                ++ show event
                ++ " instead"
      in k' tst s' ""
  Right (Left e)            -> Fail (show e)
  Left _                    -> Fail "SAX stream exhausted"
{-# INLINE bytes #-}

closeTag :: ByteString -> SaxParser ()
closeTag tag =
  tracy ("running closeTag: " ++ show tag) $
  SaxParser $ \tst s fk k ->
  case S.next s of
    Right (Right (event, s')) ->
      tracy ("closeTag " ++ show tag ++ " event: " ++ show event) $
      case event of
        CloseTag tagN ->
          tracy "close1" $
          if tagN == tag
          then
            tracy "tagN == tag" $
            k tst s' ()
          else
            tracy "else" $
            case safeHead tst of
              Nothing          ->
                Fail $ "expected a closing of tag "
                ++ show tag ++ ", got " ++ show event
                ++ " instead"
              Just (tagS,rest) ->
                if tagS == tagN
                then
                  k rest s' ()
                else
                  fk tst s
        _             ->
                  fk tst s
    Right (Left e)            ->
      tracy ("rightleft: " ++ show e) $
      Fail (show e)
    Left e                    ->
      tracy ("failing with: " ++ show e) $
      Fail "SAX stream exhausted"
{-# INLINE closeTag #-}

withTag :: ByteString -> SaxParser a -> SaxParser a
withTag tag s = do
  openTag tag
  skipUntil' (endOfOpenTag tag)
  res <- s
  closeTag tag
  pure res
{-# INLINE withTag #-}

atTag :: ByteString -> SaxParser a -> SaxParser a
atTag tag p = do
  skipUntil (openTag tag)
  skipUntil' (endOfOpenTag tag)
  res <- p
  skipUntil' (closeTag tag)
  pure res
{-# INLINE atTag #-}

skipTag :: ByteString -> SaxParser ()
skipTag tag = do
  openTag tag
  skipUntil' (closeTag tag)
  pure ()

skipUntil :: SaxParser a -> SaxParser a
skipUntil s = s <|> (skipAndMark >> skipUntil s)
{-# INLINE skipUntil #-}

-- | A version of @skipUntil@ without @skipAndMark@.
skipUntil' :: SaxParser a -> SaxParser a
skipUntil' s = s <|> (skip >> skipUntil' s)
{-# INLINE skipUntil' #-}
