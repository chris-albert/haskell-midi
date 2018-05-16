module Util where

import qualified Data.ByteString.Lazy as BSL   
import qualified Data.ByteString as BS   
import qualified Data.Binary as DB


encode :: DB.Binary a => a -> BS.ByteString
encode n = BSL.toStrict $ DB.encode n

