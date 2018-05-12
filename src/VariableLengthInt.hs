module VariableLengthInt where

import qualified Data.ByteString as BS
import qualified Data.Binary.Strict.BitGet as BG
import qualified Data.ByteString.Char8 as C8
import Data.Int
import Data.Word


parseVarLength :: BS.ByteString -> Maybe Integer
parseVarLength bs = 
  case BG.runBitGet bs getWords of 
    Left e   -> Just 1 --Nothing
    Right ws -> fst <$> (C8.readInteger $ BS.pack ws)

parseVarLength' :: BS.ByteString -> [Word8]
parseVarLength' bs = 
  case BG.runBitGet bs getWords of 
    Left e   -> []
    Right ws -> ws


getWords :: BG.BitGet [Word8]
getWords = do
  --f <- BG.getAsWord8 8
  f <- BG.getWord8
  return [f]
