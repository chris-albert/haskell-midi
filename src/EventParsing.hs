module EventParsing where

import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString as BS
import qualified Data.Binary.Strict.BitGet as BG
import qualified VariableLengthInt as VL
import qualified Event as E
import Data.Word

dispatch :: Word8 -> Word8 -> Word32 -> BG.BitGet E.Event
--dispatch s c = error $ "Invalid message: [" ++ show s ++"][" ++ show c ++ "]"
-- Midi Messages
dispatch 0X08 c l = (\(k,v) -> E.MidiEvent l $ E.NoteOff c k v) <$> extract2
dispatch 0X09 c l = (\(k,v) -> E.MidiEvent l $ E.NoteOn c k v) <$> extract2
dispatch 0X0A c l = (\(k,v) -> E.MidiEvent l $ E.PolyphonicKeyPressure c k v) <$> extract2
dispatch 0X0B c l = (\(k,v) -> E.MidiEvent l $ E.ControllerChange c k v) <$> extract2
dispatch 0X0C c l = (\k     -> E.MidiEvent l $ E.ProgramChange c k) <$> extract1
dispatch 0X0D c l = (\k     -> E.MidiEvent l $ E.ProgramChange c k) <$> extract1
dispatch 0X0E c l = (\(k,v) -> E.MidiEvent l $ E.PitchBend c k v) <$> extract2
--Sysex Messages
dispatch 0X0F 0X00 l = E.SysexEvent l <$> varLength
dispatch 0X0F 0X07 l = E.SysexEvent l <$> varLength
--Meta Messages
dispatch 0X0F 0X0F l = (\m -> E.MetaEvent l <$> dispatchMeta m) =<< BG.getWord8
dispatch s c l = error $ "Invalid message: [" ++ show s ++"][" ++ show c ++ "][" ++ show l ++"]"

getEvent :: BG.BitGet E.Event
getEvent = do
  vl    <- VL.parseVarLength
  (s,c) <- getStatus
  dispatch s c vl

getEvents :: BG.BitGet [E.Event]
getEvents = do
  empty <- BG.isEmpty
  if empty
     then return []
     else do event  <- getEvent
             events <- getEvents
             return (event:events)

extract1 :: BG.BitGet Word8
extract1 = BG.getWord8

extract2 :: BG.BitGet (Word8, Word8)
extract2 = do
  f <- BG.getWord8
  s <- BG.getWord8
  return (f,s)

getStatus :: BG.BitGet (Word8, Word8)
getStatus = do
  s <- BG.getAsWord8 4
  c <- BG.getAsWord8 4
  return (s,c)

dispatchMeta :: Word8 -> BG.BitGet E.MetaMessage
dispatchMeta 0X00 = const (E.SequenceNumber <$> BG.getWord16be) =<< BG.getWord8
dispatchMeta 0X01 = E.TextEvent <$> varLength
dispatchMeta 0X02 = E.CopyrightNotice <$> varLength
dispatchMeta 0X03 = E.TrackName <$> varLength
dispatchMeta 0X04 = E.InstrumentName <$> varLength
dispatchMeta 0X05 = E.Lyric <$> varLength
dispatchMeta 0X06 = E.Marker <$> varLength
dispatchMeta 0X07 = E.CuePoint <$> varLength
dispatchMeta 0X20 = const (E.MidiChannelPrefix <$> BG.getWord8) =<< BG.getWord8
dispatchMeta 0X2F = const E.EndOfTrack <$> BG.getWord8
dispatchMeta 0X51 = const (E.SetTempo <$> get24Bit) =<< BG.getWord8
dispatchMeta 0X54 = const (E.SMTPEOffset <$> BG.getWord8 <*> BG.getWord8 <*> BG.getWord8 <*> BG.getWord8 <*> BG.getWord8) =<< BG.getWord8
dispatchMeta 0X58 = const (E.TimeSignature <$> BG.getWord8 <*> BG.getWord8 <*> BG.getWord8 <*> BG.getWord8) =<< BG.getWord8
dispatchMeta 0X59 = const (E.KeySignature <$> BG.getWord8 <*> BG.getWord8) =<< BG.getWord8
dispatchMeta 0X7F = E.SequencerMetaEvent <$> varLength
dispatchMeta _    = error ""

varLength ::BG.BitGet BS.ByteString
varLength = varLengthByteString =<< VL.parseVarLength

varLengthByteString :: Word32 -> BG.BitGet BS.ByteString
varLengthByteString = BG.getLeftByteString . (8 *) . fromIntegral

get24Bit :: BG.BitGet Word32
get24Bit = (\a b c -> VL.getWord32 [a,b,c]) <$> BG.getWord8 <*> BG.getWord8 <*> BG.getWord8
