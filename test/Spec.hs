{-# LANGUAGE BinaryLiterals #-}

import Test.Tasty.HUnit
import Test.Tasty
import qualified VariableLengthInt  as VL
import qualified Data.ByteString.Lazy as BSL   
import qualified Data.ByteString as BS   
import qualified Data.Binary as DB
import Data.Word


import Numeric (showHex, showIntAtBase)
import Data.Char (intToDigit)
import qualified Data.ByteString.Char8 as C8


printBin :: Int -> String
printBin i = showIntAtBase 2 intToDigit i ""

main :: IO ()
main = do 
  putStrLn ""
  putStrLn "Running Tests"
  let a = encode (0X7F :: Word8)
      b = 0X7F :: Word8
  print a
  print $ fromIntegral b
  print $ BS.unpack a
  print $ C8.readInt a
  defaultMain tests

tests :: TestTree
tests = testGroup "Properties"
  [
    parse 
  ]

encode :: DB.Binary a => a -> BS.ByteString
encode n = BSL.toStrict $ DB.encode n

parse :: TestTree
parse = testGroup "Parse Var Length"
  [
    testCase "00 hex should be 0" $
      VL.parseVarLength (encode (0X00 :: Word8)) @?= Just 0,
    testCase "7F hex should be 127" $
      VL.parseVarLength (encode (0X7F :: Word8)) @?= Just 127,
    testCase "8100 hex should be 128" $
      VL.parseVarLength (encode (0X8100 :: Word16)) @?= Just 128,
    testCase "FF7F hex should be 16383" $
      VL.parseVarLength (encode (0XFF7F :: Word16)) @?= Just 16383

  ]


