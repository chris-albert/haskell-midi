module EventParsing where

import qualified Data.ByteString.Lazy as BSL
import qualified Data.Binary.Strict.BitGet as BG
import qualified Event as E
import Data.Word


parseEvents :: BSL.ByteString -> (BSL.ByteString, [E.Event])
parseEvents bs = (bs,[])
  where (e, rbs) = case BG.runBitGet (BSL.toStrict bs) getStatus of 
                     Left e      -> error $ "parseEvents: " ++ e
                     Right (s,c) -> dispatch s c (BSL.drop 2 bs)

dispatch :: Word8 -> Word8 -> BSL.ByteString -> (E.Event, BSL.ByteString)
dispatch 0x08 c bs = (E.MidiEvent $ E.NoteOff c k v, r)
  where (k,v,r) = extract2 bs 
dispatch 0x09 c bs = (E.MidiEvent $ E.NoteOn c k v, r)
  where (k,v,r) = extract2 bs
dispatch _ _ _  = error "NI"

extract2 :: BSL.ByteString -> (Word8, Word8, BSL.ByteString)
extract2 bs = (rf,rs,BSL.drop 2 bs)
  where get = do
            f <- BG.getWord8
            s <- BG.getWord8
            return (f,s)
        (rf, rs, r) = case BG.runBitGet (BSL.toStrict bs) get of
          Left e      -> error $ "extract2: " ++ e
          Right (a,b) -> (a,b,bs)

getStatus :: BG.BitGet (Word8, Word8)
getStatus = do
  s <- BG.getAsWord8 4
  c <- BG.getAsWord8 4
  return (s,c)
