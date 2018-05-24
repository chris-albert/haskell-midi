module EventInterp where

import qualified Midi as M
import qualified Event as E
import qualified Text.Printf as TP
import Control.Monad.State
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
  show (NoteOn k)          = TP.printf "Note On [%d]" k
  show (SetTempo mpq)      = TP.printf "Set Tempo [%.1f]" $ getBeatsPerMinute mpq
  show (TimeSignature n d) = TP.printf "Time Signature [%d/%d]" n (2 ^ d :: Word8)
  show (Other o)           = "Other" 

mapToInterp :: E.Event -> EventInterp
mapToInterp (E.MidiEvent _ (E.NoteOn _ k _))           = NoteOn k
mapToInterp (E.MetaEvent _ (E.SetTempo t))             = SetTempo t
mapToInterp (E.MetaEvent _ (E.TimeSignature n d _ _ )) = TimeSignature n d
mapToInterp e                                          = Other e

getBeatsPerMinute :: Word32 -> Double
getBeatsPerMinute s = 60 / (fromIntegral s / 1000000)

data EventWithTime = EventWithTime {
  time :: Word32,
  eventInterp :: EventInterp
}

instance Show EventWithTime where
  show (EventWithTime t ei) = TP.printf "%s - %s" (convertToTime t) $ show ei

mapWithTime :: (Word32, [E.Event]) -> [EventWithTime]
mapWithTime (_, []) = []
mapWithTime (time, e:es) = EventWithTime newTime (mapToInterp e) : mapWithTime (newTime,es)
  where newTime = time + E.deltaTime e

eventsToTime :: [E.Event] -> [EventWithTime]
eventsToTime es = mapWithTime (0,es)

convertToTime :: Word32 -> String
convertToTime w = show w

        
