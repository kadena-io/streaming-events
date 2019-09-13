{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module: Network.Wai.EventSource.Streaming
-- Copyright: Copyright © 2019 Kadena LLC.
-- License: BSD-3-Clause
-- Maintainer: Colin Woodbury <colin@kadena.io>
--
module Network.Wai.EventSource.Streaming
  ( -- * `ServerEvent` Stream
    withEvents
    -- * Attoparsec Parser
  , event
  ) where

import           Control.Applicative (many, optional)
import           Control.Monad (unless)
import           Data.Attoparsec.ByteString.Char8
import qualified Data.Attoparsec.ByteString.Streaming as SA
import           Data.Binary.Builder (Builder, fromByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Streaming.Char8 as SB
import           Network.HTTP.Client
import           Network.Wai.EventSource (ServerEvent(..))
import           Prelude hiding (takeWhile)
import           Streaming
import qualified Streaming.Prelude as SP

---

-- | A low-level wrapper around `withResponse`. Provides a simple, unending
-- source of `ServerEvent`s, presumably from an endpoint served via
-- `Network.Wai.EventSource.eventSourceAppIO` or otherwise.
withEvents :: Request -> Manager -> (Stream (Of ServerEvent) IO () -> IO a) -> IO a
withEvents r m f = withResponse r m $ f . g . responseBody
  where
    g :: BodyReader -> Stream (Of ServerEvent) IO ()
    g = void . SA.parsed event . SB.fromChunks . fromBodyReader

fromBodyReader :: BodyReader -> Stream (Of B.ByteString) IO ()
fromBodyReader br = do
  bs <- liftIO $ brRead br
  unless (B.null bs) $ SP.yield bs >> fromBodyReader br

--------------------------------------------------------------------------------
-- Parser

-- NOTE: This currently ignores:
--   * windows support for now, w.r.t. end-of-line characters
--   * the optional starting byte-order-mark
--   * `CommentEvent`
--   * `RetryEvent`
--   * `CloseEvent`

event :: Parser ServerEvent
event = ServerEvent
  <$> optional (string "event" *> char ':' *> chars <* eol)
  <*> optional (string "id"    *> char ':' *> chars <* eol)
  <*> many     (string "data"  *> char ':' *> chars <* eol)

chars :: Parser Builder
chars = fromByteString <$> takeTill (== '\n')

eol :: Parser Char
eol = char '\n'
