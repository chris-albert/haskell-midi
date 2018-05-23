module EventInterp where

import qualified Midi as M
import qualified Event as E
import Data.Word

data EventInterp =
  NoteOn {
    key :: Word8
  } |
  SetTempo {
    microPerQuarter :: Word32
  } |
  TimeSignature {
    numerator :: Word8,
    denominator :: Word8
  } |
  Other {
    event :: E.Event
  }

instance Show EventInterp where
  show (NoteOn k)          = "Note On"
  show (SetTempo mpq)      = "Set Tempo"
  show (TimeSignature n d) = "Time Signature"
  show (Other o)           = "Other" 

mapToInterp :: E.Event -> EventInterp
mapToInterp (E.MidiEvent _ (E.NoteOn _ k _))           = NoteOn k
mapToInterp (E.MetaEvent _ (E.SetTempo t))             = SetTempo t
mapToInterp (E.MetaEvent _ (E.TimeSignature n d _ _ )) = TimeSignature n d
mapToInterp e                                          = Other e

getBeatsPerMinute :: Word32 -> Double
getBeatsPerMinute s = 60 / (fromIntegral s / 1000000)
