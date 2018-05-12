module Division where

import qualified Data.ByteString as BS
import qualified Data.Binary.Strict.BitGet as BG
import Data.Int
import Data.Word

data Division =
  Ticks
    {
      ticksPerQuarterNote :: Word16
    }
  |
  Frames
    {
      framesPerSec  :: Word8,
      ticksPerFrame :: Word8
    }
  deriving (Show)

parseDivision :: BS.ByteString -> Division
parseDivision d = 
  let bitGet = do
        t <- BG.getBit --Decider bit for Division (0 = Ticks, 1 = Frames)
        if t then parseFrames else parseTicks
  in case BG.runBitGet d bitGet of
    Left e -> error "Error parsing Division"
    Right d -> d

parseTicks :: BG.BitGet Division
parseTicks = Ticks <$> BG.getAsWord16 15

parseFrames :: BG.BitGet Division
parseFrames = do
  fs <- BG.getAsWord8 7
  tf <- BG.getWord8
  return $ Frames fs tf
