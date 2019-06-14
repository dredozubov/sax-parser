{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}

module SAX
  ( SaxStream
  , Result(..)
  , SaxParser(..)
  , AnyNS(..)
  , parseSax
  , skip
  , skipAndMark
  , openTag
  , openTag'
  , endOfOpenTag
  , endOfOpenTag'
  , bytes
  , closeTag
  , closeTag'
  , attr
  , attr'
  , anyAttr
  , skipUntil
  , withTag
  , withTag'
  , withTags
  , withTags'
  , withTagAndAttrs
  , withTagAndAttrs'
  , withAttrs
  , skipTag
  , skipTag'
  , skipAttr
  , atTag
  , atTag'
  , streamXml
  , peek
  ) where


import           Control.Applicative
import           Control.Monad.Fail
import           Data.ByteString hiding (empty)
import           Data.Semigroup hiding (Any)
import           Data.String
import           Data.Word
import           Debug.Tracy
import           Prelude hiding (fail, concat, span)
import           SAX.Streaming
import           Streaming hiding ((<>))
import qualified Streaming.Prelude as S
import           Xeno.Types


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

type SaxStream = Stream (Of SaxEvent) (Either XenoException) ()

parseSax :: SaxParser a -> SaxStream -> Result a
parseSax (SaxParser p) s = p [] s (\_ _ -> Fail "fail handler") (\_ _ a -> Done a)
{-# INLINE parseSax #-}

-- | Shows current @SaxEvent@.
peek :: SaxParser SaxEvent
peek = SaxParser $ \tst s _ k ->
  case S.next s of
    Right (Right (event, _)) -> k tst s event
    Right (Left _)           -> Fail "peek: empty sax stream"
    Left _                   -> Fail "SAX stream exhausted"
{-# INLINE peek #-}

peekSkiplist :: SaxParser [ByteString]
peekSkiplist = SaxParser $ \tst s _ k -> k tst s tst

safeHead :: [a] -> Maybe (a, [a])
safeHead [] = Nothing
safeHead (a:as) = Just (a, as)
{-# INLINE safeHead #-}

-- | Implies a name with an arbitrary XML namespace.
newtype AnyNS = AnyNS ByteString
  deriving (Show, Eq, Ord)

class Show a => XMLName a where
  compareNames :: a -> ByteString -> Bool

instance {-# OVERLAPPABLE #-} a ~ ByteString => XMLName a where
  compareNames = (==)
  {-# INLINE compareNames #-}

instance {-# OVERLAPPING #-} XMLName AnyNS where
  compareNames (AnyNS bs) name = bs == snd (breakEnd (== colon) name)
    where
      colon :: Word8
      colon = 58
  {-# INLINE compareNames #-}

skip :: SaxParser ()
skip = SaxParser $ \tst s _ k ->
  case S.next s of
    Right (Right (_, s')) -> k tst s' ()
    _                     -> Fail "skip: stream exhausted"
{-# INLINE skip #-}

skipAndMark :: SaxParser ()
skipAndMark = SaxParser $ \tst s _ k ->
  case S.next s of
    Right (Right (event, s')) ->
      case event of
        OpenTag tag -> k (tag:tst) s' ()
        _           -> k tst s' ()
    _                         -> Fail "skip: stream exhausted"
{-# INLINE skipAndMark #-}

openTag :: ByteString -> SaxParser ()
openTag = openTag'
{-# INLINE openTag #-}

openTag' :: XMLName name => name -> SaxParser ()
openTag' tag = SaxParser $ \tst s fk k ->
  case S.next s of
    Right (Right (event, s')) ->
      case event of
        OpenTag tagN -> if compareNames tag tagN then k tst s' () else fk tst s
        e            -> case safeHead tst of
          Nothing -> fk tst s
          Just (tagS,rest) -> if e == CloseTag tagS
            then k rest s' ()
            else fk tst s
    Right (Left e)            -> Fail (show e)
    Left _                    -> Fail "SAX stream exhausted"
{-# INLINE openTag' #-}

openAndMark :: SaxParser ByteString
openAndMark = SaxParser $ \tst s fk k ->
  case S.next s of
    Right (Right (event, s')) ->
      case event of
        OpenTag tagN -> k (tagN:tst) s' tagN
        _            -> fk tst s
    Right (Left e)            -> Fail (show e)
    Left _                    -> Fail "SAX stream exhausted"
{-# INLINE openAndMark #-}

endOfOpenTag
  :: ByteString
  -> SaxParser ()
endOfOpenTag = endOfOpenTag'
{-# INLINE endOfOpenTag #-}

endOfOpenTag' :: XMLName name => name -> SaxParser ()
endOfOpenTag' tag = SaxParser $ \tst s fk k ->
  case S.next s of
   Right (Right (event, s')) ->
     case event of
       EndOfOpenTag tagN -> if compareNames tag tagN then k tst s' () else fk tst s
       e                 -> case safeHead tst of
         Nothing          -> fk tst s
         Just (tagS,rest) -> if e == CloseTag tagS
           then k rest s' ()
           else fk tst s
   Right (Left e)            -> Fail (show e)
   Left _                    -> Fail "SAX stream exhausted"
{-# INLINE endOfOpenTag' #-}

bytes :: SaxParser ByteString
bytes = SaxParser $ \tst s fk k -> case S.next s of
  Right (Right (event, s')) ->
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
closeTag = closeTag'
{-# INLINE closeTag #-}

closeTag'
  :: XMLName name
  => name
  -> SaxParser ()
closeTag' tag = SaxParser $ \tst s fk k ->
  case S.next s of
    Right (Right (event, s')) -> tracy ("CloseTag event: "++ show event) $ case event of
      CloseTag tagN ->
        case safeHead tst of
          Nothing          -> tracy "safeHead - Nothing" $
            if let x = compareNames tag tagN in tracy (show x) x
            then k tst s' ()
            else fk tst s
          Just (tagS,rest) -> tracy ("safeHead - Just "++show tagS) $
            if let x = compareNames tagS tagN in tracy (show x) x
            then tracy ("tst: "++ show (s', rest)) $ k rest s' ()
            else if compareNames tag tagN then k tst s' () else fk tst s
        -- if compareNames tag tagN then k tst s' () else case safeHead tst of
        --   Nothing          -> fk tst s
        --   Just (tagS,rest) -> if tagS == tagN then k rest s' () else fk tst s
      _           ->
                fk tst s
    Right (Left e)            -> Fail (show e)
    Left _                    -> Fail "SAX stream exhausted"
{-# INLINE closeTag'#-}

attr
  :: ByteString
  -> SaxParser ByteString
attr = attr'
{-# INLINE attr #-}

attr'
  :: XMLName name
  => name
  -> SaxParser ByteString
attr' name = SaxParser $ \tst s fk k ->
  case S.next s of
    Right (Right (event, s')) -> case event of
      Attr nameN val -> if compareNames name nameN then k tst s' val else fk tst s
      _              -> fk tst s
    Right (Left e)            -> Fail (show e)
    Left _                    -> Fail "SAX stream exhausted"
{-# INLINE attr' #-}

anyAttr :: SaxParser (ByteString, ByteString)
anyAttr = SaxParser $ \tst s fk k ->
  case S.next s of
    Right (Right (event, s')) -> case event of
      Attr name val -> k tst s' (name, val)
      _              -> fk tst s
    Right (Left e)            -> Fail (show e)
    Left _                    -> Fail "SAX stream exhausted"
{-# INLINE anyAttr #-}

skipAttr :: SaxParser ()
skipAttr = SaxParser $ \tst s fk k ->
  case S.next s of
    Right (Right (event, s')) -> case event of
      Attr _ _ -> k tst s' ()
      _        -> fk tst s
    Right (Left e)            -> Fail (show e)
    Left _                    -> Fail "SAX stream exhausted"
{-# INLINE skipAttr #-}

withTag
  :: ByteString
  -> SaxParser a
  -> SaxParser a
withTag = withTag'
{-# INLINE withTag #-}

-- | Parses tag with its content, skipping the attributes.
withTag'
  :: XMLName name
  => name
  -> SaxParser a
  -> SaxParser a
withTag' tag s = do
  tracyM $ "==== withTag " ++ show tag ++ " ===="
  openTag' tag
  tracyM $ "==== withTag opened " ++ show tag ++ " ===="
  skipUntil' (endOfOpenTag' tag)
  tracyM $ "==== withTag skipped " ++ show tag ++ " ===="
  res <- s
  tracyM $ "==== withTag parsed " ++ show tag ++ " ===="
  closeTag' tag
  tracyM $ "==== withTag closed " ++ show tag ++ " ===="
  pure res
{-# INLINE withTag' #-}

withTags
  :: [ByteString]
  -> SaxParser a
  -> SaxParser a
withTags = withTags'
{-# INLINE withTags #-}

-- | Allows to nest @withTag@ parsers in a more concise way.
withTags'
  :: XMLName name
  => [name]
  -> SaxParser a
  -> SaxParser a
withTags' [] s = s
withTags' (tag:tl) s = withTag' tag (withTags' tl s)
{-# INLINE withTags' #-}

withAttrs
  :: ByteString
  -> SaxParser attrs
  -> SaxParser attrs
withAttrs = withAttrs'
{-# INLINE withAttrs #-}

withAttrs'
  :: XMLName name
  => name
  -> SaxParser attrs
  -> SaxParser attrs
withAttrs' tag sattrs = do
  openTag' tag
  attrs <- sattrs
  endOfOpenTag' tag
  skipUntil' (closeTag' tag)
  pure attrs
{-# INLINE withAttrs' #-}

withTagAndAttrs
  :: ByteString
  -> SaxParser attrs
  -> SaxParser a
  -> SaxParser (attrs, a)
withTagAndAttrs = withTagAndAttrs'
{-# INLINE withTagAndAttrs #-}

withTagAndAttrs'
  :: XMLName name
  => name
  -> SaxParser attrs
  -> SaxParser a
  -> SaxParser (attrs, a)
withTagAndAttrs' tag sattrs sa = do
  openTag' tag
  attrs <- sattrs
  endOfOpenTag' tag
  res <- sa
  closeTag' tag
  pure (attrs, res)
{-# INLINE withTagAndAttrs' #-}

atTag
  :: ByteString
  -> SaxParser a
  -> SaxParser a
atTag = atTag'
{-# INLINE atTag #-}

atTag'
  :: XMLName name
  => name
  -> SaxParser a
  -> SaxParser a
atTag' tag p = do
  tracyM "entered atTag'"
  withTag' tag p <|> (do
    tracyM ("atTag': ")
    t <- openAndMark
    tracyM ("atTag' opened and marked ")
    skipUntil (closeTag' t)
    tracyM ("atTag' skipped: ")
    atTag' tag p
    )
{-# INLINE atTag' #-}

-- | Skips a tag and all of its children.
skipTag :: ByteString -> SaxParser ()
skipTag = skipTag'

skipTag'
  :: XMLName name
  => name
  -> SaxParser ()
skipTag' tag = do
  openTag' tag
  skipUntil' (closeTag' tag)
  pure ()
{-# INLINE skipTag' #-}

skipUntil :: SaxParser a -> SaxParser a
skipUntil s = s <|> (skipAndMark >> skipUntil s)
{-# INLINE skipUntil #-}

-- | A version of @skipUntil@ without @skipAndMark@.
skipUntil' :: SaxParser a -> SaxParser a
skipUntil' s = s <|> (skip >> skipUntil' s)
{-# INLINE skipUntil' #-}
