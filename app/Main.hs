module Main where
import qualified Midi
import qualified Args
import qualified EventInterp as EI
import Data.Word

main :: IO ()
main = do
  args <- Args.getArgs
  out <- Midi.parseMidi args
  --print $ head out
  mapM_ print $ getEvents 10 out

getEvents :: Int -> [Midi.Chunk] -> [String]
getEvents i cs = concat $ getEvent i <$> cs 
      
getEvent :: Int -> Midi.Chunk -> [String]
getEvent _ h @ Midi.Header{} = [show h]
getEvent i (Midi.Track _ events)     = show <$> EI.eventsToTime 9600 (take i events)

