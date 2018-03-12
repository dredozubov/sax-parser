{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE BangPatterns #-}

module SAX.Streaming where

import           Control.Monad.Except
import           Data.ByteString (ByteString)
import           Streaming
import qualified Streaming.Prelude as S
import           Xeno.SAX
import           Xeno.Types


data SaxEvent
  = OpenTag !ByteString
  | Attr !ByteString !ByteString
  | EndOfOpenTag !ByteString
  | Text !ByteString
  | CloseTag !ByteString
  | CDATA !ByteString
  deriving (Show, Eq, Ord)

streamXml
  :: (MonadError XenoException m)
  => ByteString
  -> Stream (Of SaxEvent) m ()
streamXml str = process
  (S.yield . OpenTag)
  (\a b -> S.yield $ Attr a b)
  (S.yield . EndOfOpenTag)
  (S.yield . Text)
  (S.yield . CloseTag)
  (S.yield . CDATA)
  str
