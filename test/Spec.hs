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
      VL.parseVarLength (encode (0XFF7F :: Word16)) @?= Just 16383,
    testCase "8768 hex should be 1000" $
      VL.parseVarLength (encode (0X8768 :: Word16)) @?= Just 1000,
    testCase "BD8440 hex should be 1000000" $
      VL.parseVarLength (encode (0X80BD8440 :: Word32)) @?= Just 1000000,
    testCase "FFFFFF7F hex should be 268435455" $
      VL.parseVarLength (encode (0XFFFFFF7F :: Word32)) @?= Just 268435455
  ]


