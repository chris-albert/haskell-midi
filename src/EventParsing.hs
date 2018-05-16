module EventParsing where

import qualified Data.ByteString.Lazy as BSL

import qualified Event as E


parseEvents :: BSL.ByteString -> (BSL.ByteString, [E.Event])
parseEvents = error "NI"
