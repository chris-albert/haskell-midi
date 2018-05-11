module Midi where

import qualified Data.ByteString.Lazy as BSL
import qualified Args
import qualified Data.ByteString.Lazy.Char8 as C8
import qualified Data.Binary.Get as BG
import qualified Data.Word as DW
import Data.Int

data ChunkType = Header | Track deriving (Show)

data Chunk = Chunk
  {
    chunkType :: ChunkType,
    length    :: Int32,
    body      :: BSL.ByteString
  } 
  deriving (Show)

--data Chunk2 = 
--  Header 
--    {
--      length :: Int32
--    }
--  |
--  Track
--    {
--      length :: Int32
--    }
--  deriving (Show)

parseChunkType :: String -> ChunkType
parseChunkType "\"MThd\"" = Header
parseChunkType "\"MTrk\"" = Track
parseChunkType ct         = error $ "Invalid chunk type [" ++ ct ++ "]"

renderChunk :: Chunk -> String
renderChunk c = (show $ chunkType c) ++ " [" ++ (show $ Midi.length c) ++ "]" 

parseMidi :: Args.MidiArgs -> IO [String]
parseMidi filename = do
  bs <- BSL.readFile $ Args.file filename
  let chunks = toChunks bs
  return $ fmap renderChunk chunks

parseHeader :: BSL.ByteString -> String 
parseHeader _ = error "NI"

parseChunk :: BG.Get Chunk
parseChunk = do 
  chunk  <- BG.getByteString 4
  length <- BG.getInt32be
  body   <- BG.getLazyByteString $ fromIntegral length
  let ct  = parseChunkType $ show chunk
  return $ Chunk ct length body

parseChunks :: BG.Get [Chunk]
parseChunks = do
  empty <- BG.isEmpty
  if empty
     then return []
     else do chunk  <- parseChunk
             chunks <- parseChunks
             return (chunk:chunks)

toChunks :: BSL.ByteString -> [Chunk]
toChunks = BG.runGet parseChunks
