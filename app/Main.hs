module Main where
import qualified Midi
import qualified Args

main :: IO ()
main = do
  args <- Args.getArgs
  out <- Midi.parseMidi args
  print $ head out
  --mapM_ print out

