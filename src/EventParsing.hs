module EventParsing where

import qualified Data.ByteString.Lazy as BSL
import qualified Data.Binary.Strict.BitGet as BG
import qualified Event as E
import Data.Word

dispatch :: Word8 -> Word8 -> BG.BitGet E.Event
dispatch 0X08 c = (\(k,v) -> E.MidiEvent $ E.NoteOff c k v) <$> extract2
dispatch 0X09 c = (\(k,v) -> E.MidiEvent $ E.NoteOn c k v) <$> extract2
dispatch _ _ = error "NI"

getEvent :: BG.BitGet E.Event
getEvent = do
  (s,c) <- getStatus
  dispatch s c

getEvents :: BG.BitGet [E.Event]
getEvents = do
  empty <- BG.isEmpty
  if empty
     then return []
     else do event  <- getEvent
             events <- getEvents
             return (event:events)

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
