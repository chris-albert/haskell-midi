module Midi where

import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString as BS
import qualified Args
import qualified Data.ByteString.Lazy.Char8 as C8
import qualified Data.Word as DW
import qualified Data.Bits as B
import qualified Data.Binary.Strict.BitGet as BG
import qualified Division as D
import qualified VariableLengthInt as VL
import qualified Event as E
import qualified EventParsing as EP
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
      length    :: Word32,
      deltaTime :: Word32,
      event     :: [E.Event]
    }
  |
  Invalid
  deriving (Show)

parseHeader :: Word32 -> BG.BitGet Chunk
parseHeader l = (\(f,t,d) -> Header l f t $ D.parseDivision d) <$> get
  where get = do
            format   <- BG.getWord16be
            tracks   <- BG.getWord16be
            division <- BG.getLeftByteString 16
            return (format,tracks,division)

parseTrack :: Word32 -> BG.BitGet Chunk
parseTrack l = do
  vl <- VL.parseVarLength
  --es <- EP.getEvents
  return $ Track l vl []--es


parseChunkType :: String -> Word32 -> BG.BitGet Chunk
parseChunkType "\"MThd\"" l = parseHeader l
parseChunkType "\"MTrk\"" l = parseTrack l
parseChunkType ct _         = return Invalid --error $ "Invalid chunk type [" ++ ct ++ "]"

renderChunk :: Either String [Chunk] -> [String]
renderChunk (Right cs) = show <$> cs
renderChunk (Left e)   = ["Error: " ++ e]

parseMidi :: Args.MidiArgs -> IO [String]
parseMidi filename = do
  bs <- BSL.readFile $ Args.file filename
  return $ renderChunk $ toChunks bs           
  
parseChunk :: BG.BitGet Chunk
parseChunk = do 
  chunk  <- BG.getLeftByteString 32
  length <- BG.getWord32be
  parseChunkType (show chunk) length

parseChunks :: BG.BitGet [Chunk]
parseChunks = do
  empty <- BG.isEmpty
  if empty
     then return []
     else do chunk  <- parseChunk
             chunks <- parseChunks
             return (chunk:chunks)

toChunks :: BSL.ByteString -> Either String [Chunk]
toChunks bs = BG.runBitGet (BSL.toStrict bs) parseChunks
