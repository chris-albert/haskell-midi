module EventInterp where

import qualified Midi as M
import qualified Event as E
import qualified Text.Printf as TP
import qualified Data.Time.Units as DTU
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
  time :: Double,
  eventInterp :: EventInterp
}

instance Show EventWithTime where
  show (EventWithTime t ei) = TP.printf "%s - %s" (convertToTime t) $ show ei

mapWithTime :: Word16 -> Double -> Double -> [E.Event] -> [EventWithTime]
mapWithTime tpq _ _ []         = []
mapWithTime tpq ct ticks (e:es) = EventWithTime newTime (mapToInterp e) : mapWithTime tpq ct newTime es
  where tempo = case e of
                  E.MetaEvent _ (E.SetTempo t) -> fromIntegral t / fromIntegral tpq
                  _ -> ct
        newTicks = ticks + fromIntegral (E.deltaTime e)
        newTime = newTicks * tempo

eventsToTime :: Word16 -> [E.Event] -> [EventWithTime]
eventsToTime tpq = mapWithTime tpq 10 0

convertToTime :: Double -> String
convertToTime w = show s
  where ms = DTU.fromMicroseconds (floor w :: Integer) :: DTU.Microsecond
        mis = DTU.convertUnit ms :: DTU.Millisecond 
        s   = DTU.convertUnit ms :: DTU.Second
        m   = DTU.convertUnit ms :: DTU.Minute
        h   = DTU.convertUnit ms :: DTU.Hour
 

        
