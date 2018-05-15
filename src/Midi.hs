module Midi where

import qualified Data.ByteString.Lazy as BSL
import qualified Args
import qualified Data.ByteString.Lazy.Char8 as C8
import qualified Data.Binary.Get as BG
import qualified Data.Word as DW
import qualified Data.Bits as B
import qualified Data.Binary.Strict.BitGet as BiG
import qualified Division as D
import qualified VariableLengthInt as VL
import Data.Int
import Data.Word

data Chunk = 
  Header 
    {
      length   :: Word32,
      format   :: Word16,
      tracks   :: Word16,
      division :: D.Division
    }
  |
  Track
    {
      length :: Word32,
      actualLength :: Int64,
      deltaTime :: Maybe Word32,
      body :: BSL.ByteString
    }
  deriving (Show)

parseHeader :: Word32 -> BSL.ByteString -> Chunk
parseHeader l bs =
  let get = do
          format   <- BG.getWord16be
          tracks   <- BG.getWord16be
          division <- BG.getByteString 2
          return (format,tracks,division)
      (f,t,d) = BG.runGet get bs
   in Header l f t $ D.parseDivision d 

parseTrack :: Word32 -> BSL.ByteString -> Chunk
parseTrack l bs = Track l (BSL.length bs) dt bs 
  where vl = VL.parseVarLength $ BSL.toStrict bs
        dt = fst <$> vl

parseChunkType :: String -> Word32 -> BSL.ByteString -> Chunk
parseChunkType "\"MThd\"" l bs = parseHeader l bs
parseChunkType "\"MTrk\"" l bs = parseTrack l bs
parseChunkType ct _ _         = error $ "Invalid chunk type [" ++ ct ++ "]"

renderChunk :: Chunk -> String
renderChunk b @ (Header _ _ _ _) = show b
renderChunk (Track l al dt b)  = "Track {length = " ++ (show l) ++ ", deltaTime = " ++ (show dt) ++", actualLength = " ++ (show al) ++ "}"

parseMidi :: Args.MidiArgs -> IO [String]
parseMidi filename = do
  bs <- BSL.readFile $ Args.file filename
  let chunks = toChunks bs
  return $ fmap renderChunk chunks

parseChunk :: BG.Get Chunk
parseChunk = do 
  chunk  <- BG.getByteString 4
  length <- BG.getWord32be
  body   <- BG.getLazyByteString $ fromIntegral length
  return $ parseChunkType (show chunk) length body

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
