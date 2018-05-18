module EventParsingSpec where

import Test.Tasty.HUnit
import Test.Tasty
import qualified EventParsing as EP
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString as BS
import qualified Data.Binary.Strict.BitGet as BG
import Data.Word
import Util


tests = testGroup "Event Parsing"
  [
    testCase "90 should be 9 and 0" $
      BG.runBitGet (encode (0X90 :: Word8)) EP.getStatus @?= Right (9 :: Word8,0 :: Word8),
    testCase "81 should be 8 and 1" $
      BG.runBitGet (encode (0X81 :: Word8)) EP.getStatus @?= Right (8 :: Word8,1 :: Word8),
    testCase "FF should be 15 and 15" $
      BG.runBitGet (encode (0XFF :: Word8)) EP.getStatus @?= Right (15 :: Word8,15 :: Word8),
    testCase "A8 should be 10 and 8" $
      BG.runBitGet (encode (0XA8 :: Word8)) EP.getStatus @?= Right (10 :: Word8,8 :: Word8) 
  ]


