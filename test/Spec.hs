{-# LANGUAGE BinaryLiterals #-}

import Test.Tasty.HUnit
import Test.Tasty
import qualified VariableLengthInt  as VL
import qualified Data.ByteString.Lazy as BSL   
import qualified Data.ByteString as BS   
import qualified Data.Binary as DB
import Data.Word


printBoolArray :: [Bool] -> String
printBoolArray b = (\x -> if x then '1' else '0') <$> b

printRuler :: String
printRuler = concat $ const "76543210" <$> [0..3]

main :: IO ()
main = do 
  putStrLn ""
  putStrLn "Running Tests"
  defaultMain tests

tests :: TestTree
tests = testGroup "Properties"
  [
    parse,
    parseWithRemainder 
  ]

encode :: DB.Binary a => a -> BS.ByteString
encode n = BSL.toStrict $ DB.encode n

parse :: TestTree
parse = testGroup "Parse Var Length (No remainder bytes)"
  [
    testCase "00 hex should be 0" $
      VL.parseVarLength (encode (0X00 :: Word8)) @?= (0, BS.empty),
    testCase "7F hex should be 127" $
      VL.parseVarLength (encode (0X7F :: Word8)) @?= (127, BS.empty),
    testCase "8100 hex should be 128" $
      VL.parseVarLength (encode (0X8100 :: Word16)) @?= (128, BS.empty),
    testCase "FF7F hex should be 16383" $
      VL.parseVarLength (encode (0XFF7F :: Word16)) @?= (16383, BS.empty),
    testCase "8768 hex should be 1000" $
      VL.parseVarLength (encode (0X8768 :: Word16)) @?= (1000, BS.empty),
    testCase "BD8440 hex should be 1000000" $
      VL.parseVarLength (encode (0X80BD8440 :: Word32)) @?= (1000000, BS.empty),
    testCase "FFFFFF7F hex should be 268435455" $
      VL.parseVarLength (encode (0XFFFFFF7F :: Word32)) @?= (268435455, BS.empty)
  ]

parseWithRemainder :: TestTree
parseWithRemainder = testGroup "Parse var length (With remainder bytes)"
  [
    testCase "00 hex should be 0" $
      VL.parseVarLength (encode (0X00FF :: Word16)) @?= (0, encode (0XFF :: Word8)),
    testCase "8100 hex should be 128" $
      VL.parseVarLength (encode (0X8100FFFF :: Word32)) @?= (128, encode (0XFFFF :: Word16)),
    testCase "FFFFFF7F hex should be 268435455" $
      VL.parseVarLength (encode (0XFFFFFF7F01FFAABB :: Word64)) @?= (268435455, encode (0X01FFAABB :: Word32))
  ]
