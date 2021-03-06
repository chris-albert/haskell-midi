
module VariableLengthInt where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Binary.Strict.BitGet as BG
import qualified Data.ByteString.Char8 as C8
import qualified Data.Binary as DB
import qualified Data.List as L
import qualified Data.Bits as B
import Data.Int
import Data.Word

parseVarLength :: BG.BitGet Word32
parseVarLength = do
  ws <- getWords
  return $ removeContinueBits $ getWord32 ws

getWords :: BG.BitGet [Word8]
getWords = do
  c <- BG.getBit
  f <- BG.getAsWord8 7
  if c
     then (f:) <$> getWords
     else return [f] 

padWord :: [Word8] -> [Word8]
padWord []      = [0,0,0,0]
padWord [x]     = [0,0,0,x]
padWord [x,y]   = [0,0,x,y]
padWord [x,y,z] = [0,x,y,z]
padWord x       = x

getWord32 :: [Word8] -> Word32
getWord32 = DB.decode . BSL.fromStrict . BS.pack . padWord

removeContinueBits :: Word32 -> Word32
removeContinueBits w = convertFromBits $ [False,False,False,False] ++ removed
  where bits = convertToBits w
        removed = deleteAt 0 $ deleteAt 8 $ deleteAt 16 $ deleteAt 24 bits

convertToBits :: Word32 -> [Bool]
convertToBits w = B.testBit w <$> [31,30..0]

convertFromBits :: [Bool] -> Word32
convertFromBits bs = L.foldl' (\acc (i,b) -> if b then B.setBit acc i else acc) 0 $ zip [31,30..0] bs

deleteAt :: Int -> [a] -> [a]
deleteAt idx l = f ++ tail s
  where (f,s) = L.splitAt idx l 
